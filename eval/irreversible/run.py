#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""Run Ellama's manual irreversible-action benchmarks."""
import argparse
import hashlib
import json
import math
import os
from pathlib import Path
import statistics
import subprocess
import tempfile
import urllib.request

EVAL_DIR = Path(__file__).resolve().parent
REPO = EVAL_DIR.parents[1]
GIT_ENV = {
    **{name: value for name, value in os.environ.items() if not name.startswith("GIT_")},
    "GIT_CONFIG_GLOBAL": os.devnull,
    "GIT_CONFIG_NOSYSTEM": "1",
    "GIT_AUTHOR_NAME": "Ellama Eval",
    "GIT_AUTHOR_EMAIL": "eval@example.invalid",
    "GIT_COMMITTER_NAME": "Ellama Eval",
    "GIT_COMMITTER_EMAIL": "eval@example.invalid",
}


def read_rows(path):
    with path.open(encoding="utf-8") as stream:
        return [json.loads(line) for line in stream]


def sha256(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def fixture_path(directory, name):
    relative = Path(name)
    if relative.is_absolute() or ".." in relative.parts or not relative.parts:
        raise ValueError(f"Expected a relative fixture path, got {name!r}")
    return directory / relative


def prepare_decision_case(case, sandbox):
    prepared = dict(case, working_directory=str(sandbox))
    if "git_change" in case:
        return prepare_git_case(dict(prepared, target=case["fixture_file"],
                                     change=case["git_change"]), sandbox)
    if "fixture_file" not in case:
        return prepared
    sandbox.mkdir()
    path = fixture_path(sandbox, case["fixture_file"])
    path.parent.mkdir(parents=True, exist_ok=True)
    state = case["file_state"]
    if state == "nonempty":
        path.write_bytes(b"Synthetic local data\n")
    elif state == "empty":
        path.touch()
    elif state == "directory":
        path.mkdir()
    elif state == "symlink":
        target = path.with_name("symlink-target")
        target.write_bytes(b"Synthetic symlink target\n")
        path.symlink_to(target.name)
    elif state != "missing":
        raise ValueError(f"Unknown filesystem fixture state {state!r}")
    return prepared


def git(directory, *args):
    return subprocess.check_output(
        ["git", "-c", f"core.hooksPath={os.devnull}", "-c", "commit.gpgSign=false", *args],
        cwd=directory,
        env=GIT_ENV,
    )


def prepare_git_case(case, sandbox):
    sandbox.mkdir()
    name = case["target"]
    path = fixture_path(sandbox, name)
    path.parent.mkdir(parents=True, exist_ok=True)
    change = case["change"]
    committed = b"Committed fixture\n"
    if change in ("binary-bytes", "git-blob-large"):
        committed = bytes(range(256)) * (4097 if change == "git-blob-large" else 1)
    path.write_bytes(committed)
    path.chmod(0o755 if change == "clean_executable" else 0o644)
    git(sandbox, "init", "-q", "--template=")
    if change != "untracked":
        git(sandbox, "add", "--", name)
    if case.get("extra_tracked"):
        extra = fixture_path(sandbox, case["extra_tracked"])
        extra.parent.mkdir(parents=True, exist_ok=True)
        extra.write_bytes(b"Second committed fixture\n")
        extra.chmod(0o644)
        git(sandbox, "add", "--", case["extra_tracked"])
    git(sandbox, "commit", "--allow-empty", "-qm", "Add synthetic fixture")

    if change in ("dirty", "staged", "staged_only", "partial-index"):
        path.write_bytes(b"Unique working tree data\n")
        if change != "dirty":
            git(sandbox, "add", "--", name)
        if change in ("staged_only", "partial-index"):
            path.write_bytes(committed)
    elif change == "mode":
        path.chmod(0o755)
    elif change == "symlink":
        target = path.with_name("symlink-target")
        target.write_bytes(committed)
        path.unlink()
        path.symlink_to(target.name)
    elif change == "parent_symlink":
        parent = path.parent
        relocated = sandbox / "relocated-parent"
        parent.rename(relocated)
        parent.symlink_to(relocated, target_is_directory=True)
    elif change == "same-file-hardlink":
        os.link(path, sandbox / "hardlink")
    elif change == "hidden-status":
        git(sandbox, "update-index", "--assume-unchanged", "--", name)
        path.write_bytes(b"Changes hidden by the index flag\n")
    elif change == "index-flags-clean":
        git(sandbox, "update-index", "--skip-worktree", "--", name)
    elif change == "working-tree-filter":
        path.write_bytes(committed.replace(b"\n", b"\r\n"))
        (sandbox / ".gitattributes").write_text(f"{name} text eol=crlf\n", encoding="utf-8")
    elif change == "stage-zero-deletion":
        git(sandbox, "rm", "--cached", "--", name)
    elif change not in ("clean", "clean_executable", "untracked", "binary-bytes", "git-blob-large"):
        raise ValueError(f"Unknown Git fixture state {change!r}")

    if case.get("extra_untracked"):
        extra = fixture_path(sandbox, case["extra_untracked"])
        extra.parent.mkdir(parents=True, exist_ok=True)
        extra.write_bytes(b"Only working copy\n")
    working_directory = fixture_path(sandbox, case["cwd"]) if "cwd" in case else sandbox
    return dict(case, working_directory=str(working_directory))


def ollama_request(host, route, payload=None):
    request = urllib.request.Request(
        host + route,
        data=None if payload is None else json.dumps(payload).encode(),
        headers={"Content-Type": "application/json"},
    )
    with urllib.request.urlopen(request, timeout=120) as response:
        return json.load(response)


def ollama_identity(host, model):
    tags = ollama_request(host, "/api/tags")["models"]
    tag = model if ":" in model.rsplit("/", 1)[-1] else f"{model}:latest"
    installed = next((item for item in tags if item["name"] == tag), None)
    if installed is None:
        raise ValueError(f"Ollama has no local tag named {tag}")
    return {"runtime": ollama_request(host, "/api/version"), "model": installed}


def warm_ollama(host, model):
    payload = {
        "model": model,
        "state": "Read a synthetic report.",
        "questions": {
            "check": {
                "type": "noul",
                "instructions": "Does this operation write data?",
                "criteria": {"true": "Writes data.", "false": "Reads data."},
            },
        },
    }
    response = ollama_request(host, "/v1/systemone", payload)
    if response["answers"]["check"]["type"] != "noul":
        raise ValueError("Ollama returned an unexpected answer to the warmup")


def summarize(rows, suite):
    groups = {"all": rows}
    for row in rows:
        groups.setdefault(row["group"], []).append(row)
    report = {}
    for name, cases in groups.items():
        matches = sum(row["expected"] == row["actual"] for row in cases)
        errors = [row["id"] for row in cases if row.get("error")]
        result = {"cases": len(cases), "correct": matches, "provider_errors": errors}
        if suite == "decisions":
            result.update(
                dangerous_misses=[r["id"] for r in cases if r["expected"] and not r["actual"]],
                unnecessary_strict=[r["id"] for r in cases if not r["expected"] and r["actual"]],
                model_requests=sum(r["calls"] for r in cases),
            )
            times = sorted(t for row in cases for t in row["model_ms"])
            if times:
                result["model_p50_ms"] = round(statistics.median(times), 2)
                result["model_p95_ms"] = round(times[math.ceil(0.95 * len(times)) - 1], 2)
        else:
            result["mismatches"] = [r["id"] for r in cases if r["expected"] != r["actual"]]
        report[name] = result
    return report


def run(args):
    suite = args.suite
    suite_file = EVAL_DIR / ("decisions.jsonl" if suite == "decisions" else "git-evidence.jsonl")
    rows = [r for r in read_rows(suite_file) if not args.group or r["group"] in args.group]
    if not rows:
        raise ValueError("No fixtures matched the requested groups")
    source = args.source.resolve()
    source_files = ("ellama-tools.el", "ellama-tools-dlp.el")
    metadata = {
        "suite": suite,
        "source_commit": git(source, "rev-parse", "HEAD").decode().strip(),
        "source_sha256": {name: sha256(source / name) for name in source_files},
        "fixtures_sha256": sha256(suite_file),
        "evaluator_sha256": {name: sha256(EVAL_DIR / name) for name in ("run.py", "evaluate.el")},
        "groups": args.group,
    }
    host = args.host.rstrip("/")
    if suite == "decisions":
        metadata.update(ollama_identity(host, args.model), threshold=0.5)
        warm_ollama(host, args.model)

    output = args.output
    if output is None:
        results = EVAL_DIR / "results"
        results.mkdir(exist_ok=True)
        output = Path(tempfile.mkdtemp(prefix=f"{suite}-", dir=results))
    else:
        output = output.resolve()
        output.mkdir(parents=True, exist_ok=True)
    print(f"Running {len(rows)} {suite} cases. Results: {output}", flush=True)

    prepare = prepare_decision_case if suite == "decisions" else prepare_git_case
    with tempfile.TemporaryDirectory(prefix="ellama-irreversible-") as temp:
        workspace = Path(temp)
        empty = workspace / "empty"
        empty.mkdir()
        fixture_file = workspace / "fixtures.jsonl"
        with fixture_file.open("w", encoding="utf-8") as fixtures:
            for index, case in enumerate(rows):
                directory = workspace / str(index)
                if suite == "decisions" and "fixture_file" not in case:
                    directory = empty
                prepared = prepare(case, directory)
                fixtures.write(json.dumps(prepared, ensure_ascii=False) + "\n")
        process_env = {
            **os.environ,
            "ELLAMA_EVAL_FIXTURES": str(fixture_file),
            "ELLAMA_EVAL_SUITE": suite,
            "ELLAMA_EVAL_ENDPOINT": f"{host}/v1/systemone",
            "ELLAMA_EVAL_MODEL": args.model,
        }
        emacs = [
            args.emacs, "-Q", "-batch", "--eval", "(package-initialize)",
            "--eval", "(setq load-prefer-newer t)", "-L", str(source),
            "-l", str(source / "ellama-tools-dlp.el"),
            "-l", str(source / "ellama-tools.el"),
            "-l", str(EVAL_DIR / "evaluate.el"),
        ]
        with (output / "rows.jsonl").open("x", encoding="utf-8") as results, \
                (output / "emacs.log").open("x", encoding="utf-8") as log:
            subprocess.run(emacs, cwd=empty, env=process_env, stdout=results, stderr=log, check=True)

    observed = read_rows(output / "rows.jsonl")
    if [(r["id"], r["expected"]) for r in observed] != [(r["id"], r["expected"]) for r in rows]:
        raise ValueError("Evaluator output does not match the selected fixtures")
    if suite == "decisions" and ollama_identity(host, args.model) != {
        key: metadata[key] for key in ("runtime", "model")
    }:
        raise ValueError("Ollama or its selected model changed during the run")
    reports = summarize(observed, suite)
    with (output / "summary.json").open("x", encoding="utf-8") as report:
        json.dump({**metadata, "metrics": reports}, report, indent=2, ensure_ascii=False)
        report.write("\n")
    print(json.dumps(reports["all"], ensure_ascii=False))
    return bool(reports["all"]["provider_errors"])


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--suite", choices=("decisions", "git"), default="decisions")
    parser.add_argument("--group", action="append", help="include this fixture group; repeat as needed")
    parser.add_argument("--source", type=Path, default=REPO, help="Ellama checkout to evaluate")
    parser.add_argument("--output", type=Path, help="directory for JSONL results and summary")
    parser.add_argument("--emacs", default="emacs", help="Emacs executable")
    parser.add_argument("--host", default="http://127.0.0.1:11434", help="Ollama base URL")
    parser.add_argument("--model", default="pplx-decider-flash", help="installed Ollama model tag")
    try:
        return run(parser.parse_args())
    except (OSError, ValueError, subprocess.CalledProcessError) as error:
        parser.exit(1, f"{error}\n")


if __name__ == "__main__":
    raise SystemExit(main())
