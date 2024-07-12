import os
import json
import sys
from typing import Protocol, runtime_checkable

results_json = os.environ.get('RESULTS', '{}') or sys.exit(1)

try:
    results = json.loads(results_json)
except json.JSONDecodeError:
    print("Error: Unable to parse RESULTS as JSON")
    exit(1)


@runtime_checkable
class HasStatus(Protocol):
    status: str


@runtime_checkable
class HasOutputs(Protocol):
    outputs: HasStatus


def process_job(job_name: str, job_data: HasOutputs) -> int:
    """
    Returns 0 on success and 1 on error.
    """
    status = job_data.outputs.status
    print(f"Processing job: {job_name} with status: {status}")

    if status == "success":
        print(f"Job {job_name} succeeded.")
        return 0
    elif status == "failure":
        print(f"Job {job_name} failed!")
        return 1
    else:
        print(f"Job {job_name} has unknown status: {status}!")
        return 1

# Iterate over each job
exit_status = 0
for job_name, job_data in results.items():
    if not isinstance(job_data, HasOutputs):
        print(f"Unexpected shape at key {job_name}: {job_data}")
        exit(2)

    exit_status += process_job(job_name, job_data)

if exit_status > 0:
    sys.exit(3)
