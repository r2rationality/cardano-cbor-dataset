#!/usr/bin/env python3

import argparse
from dataclasses import dataclass
from datetime import datetime, timezone
import html
import json
import re
from pathlib import Path
import sys

INPUT_SUFFIX = ".input.cbor"
REPORT_VERSION = "2"
COMMIT = re.compile(r"[0-9a-f]{40}")

@dataclass
class Counts:
    passed: int = 0
    total: int = 0

def markdown(value):
    """Escape metadata and diagnostics for Markdown table cells."""
    text = html.escape(str(value)).replace("\r\n", "\n").replace("\r", "\n")
    for char in "\\`*_{}[]|":
        text = text.replace(char, "\\" + char)
    return text.replace("\n", "<br>")

def read_report(path):
    """Read a report of the current format and return it with its `results` checked."""
    report = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(report, dict):
        raise ValueError("a report must be a JSON object")
    if report.get("version") != REPORT_VERSION:
        raise ValueError(f'unsupported report version {report.get("version")!r}, expected "{REPORT_VERSION}"')
    for field in ("dataset_commit", "implementation_commit"):
        if not isinstance(report.get(field), str) or not COMMIT.fullmatch(report[field]):
            raise ValueError(f"{field} must be a full commit hash (40 lowercase hexadecimal digits)")
    if not isinstance(report.get("generated_at"), int) or isinstance(report["generated_at"], bool):
        raise ValueError("generated_at must be an integer: milliseconds since the Unix epoch")
    if not isinstance(report.get("era"), str) or not report["era"]:
        raise ValueError("era must be the name of the era of the dataset, for example conway")
    if not isinstance(report.get("protocol_version"), str):
        raise ValueError("protocol_version must be a string")
    results = report.get("results")
    if not isinstance(results, dict):
        raise ValueError("results must be a JSON object mapping sample paths to outcomes")
    for outcome in results.values():
        if outcome is not True and not isinstance(outcome, str):
            raise ValueError("each result must be true or an error string")
    return report

def table_cell(counts):
    percentage = "—"
    if counts.total > 0:
        percentage = f"{100 * counts.passed / counts.total:.2f}%"
    return f"{percentage} ({counts.passed:,}/{counts.total:,})"

def generated_at(milliseconds):
    moment = datetime.fromtimestamp(milliseconds / 1000, tz=timezone.utc)
    return moment.strftime("%Y-%m-%d %H:%M:%S UTC")

def make_report(dataset, report):
    results = report["results"]
    rules = {}
    totals = {"valid": Counts(), "invalid": Counts()}
    overall = Counts()
    samples = set()
    for path in sorted(dataset.rglob("*" + INPUT_SUFFIX)):
        if not path.is_file():
            continue
        relative = path.relative_to(dataset)
        if len(relative.parts) != 3:
            raise ValueError(f"expected rule/test/sample path: {relative}")
        rule, test, _ = relative.parts
        name = relative.as_posix()
        samples.add(name)
        category = "valid" if test == "valid" else "invalid"
        if rule not in rules:
            rules[rule] = {"valid": Counts(), "invalid": Counts()}
        for counts in (rules[rule][category], totals[category], overall):
            counts.total += 1
            if results.get(name) is True:
                counts.passed += 1
    
    if not samples:
        raise ValueError(f"no {INPUT_SUFFIX} samples found in {dataset}")
    unknown = sorted(set(results) - samples)
    if unknown:
        raise ValueError(
            f"{len(unknown)} result paths are absent from the dataset; first: {unknown[0]}"
        )

    missing = len(samples) - len(results)
    failed = overall.total - overall.passed - missing
    lines = [
        "# Cardano CBOR Decoder/Encoder Conformance Report",
        "",
        f"**Corpus:** {markdown(dataset.parent.name)} / {markdown(dataset.name)}  ",
        f"**Era:** {markdown(report['era'])}  ",
        f"**Dataset commit:** `{report['dataset_commit']}`  ",
        f"**Implementation commit:** `{report['implementation_commit']}`  ",
        f"**Protocol version:** {markdown(report['protocol_version'])}  ",
        f"**Generated:** {generated_at(report['generated_at'])}  ",
        f"**Result:** {table_cell(overall)} passed; "
        f"{failed:,} failed, {missing:,} missing results.",
        "",
        "Cells show success percentage (passed/total), including missing results in the total. "
        "Valid samples must decode and re-encode to the expected bytes. "
        "Invalid samples must be rejected. "
        "A dash means no samples.",
        "",
        "| Rule | Valid | Invalid |",
        "| --- | ---: | ---: |",
    ]
    for rule in sorted(rules):
        valid = table_cell(rules[rule]["valid"])
        invalid = table_cell(rules[rule]["invalid"])
        lines.append(f"| {markdown(rule)} | {valid} | {invalid} |")
    valid = table_cell(totals["valid"])
    invalid = table_cell(totals["invalid"])
    lines.append(f"| **Total** | **{valid}** | **{invalid}** |")
    return "\n".join(lines).rstrip() + "\n"

def main():
    parser = argparse.ArgumentParser(description="Write a CBOR dataset report to stdout.")
    parser.add_argument("dataset", type=Path, help="era directory passed to test-cbor-dataset")
    parser.add_argument("results", type=Path, help="report written by `cbor verify`")
    args = parser.parse_args()
    try:
        dataset = args.dataset.resolve()
        if not dataset.is_dir():
            raise ValueError(f"not a dataset directory: {dataset}")
        report = make_report(dataset, read_report(args.results))
        sys.stdout.write(report)
    except (OSError, ValueError) as error:
        parser.exit(2, f"error: {error}\n")

if __name__ == "__main__":
    main()
