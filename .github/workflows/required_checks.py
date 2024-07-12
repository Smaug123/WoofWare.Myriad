import os
import json
import sys
from typing import Any

results_json = os.environ.get('RESULTS', '{}') or sys.exit(1)

try:
    results = json.loads(results_json)
except json.JSONDecodeError:
    print("Error: Unable to parse RESULTS as JSON")
    exit(1)

def process_job(job_name: str, job_data: dict[str, Any]) -> int:
    """
    Returns 0 on success and 1 on error.
    """
    status = job_data['result']
    print(f"Processing job: {job_name} with status: {status}")

    if status == "success":
        print(f"Job {job_name} succeeded.")
        return 0
    elif status in {"failure", "cancelled"}:
        print(f"Job {job_name} failed: status {status}!")
        return 1
    else:
        print(f"Job {job_name} has unknown status: {status}!")
        return 1

# Iterate over each job
exit_status = 0
for job_name, job_data in results.items():
    if not isinstance(job_data, dict):
        print(f"Unexpected shape at key {job_name}: {job_data}")
        sys.exit(2)
    exit_status += process_job(job_name, job_data)

if exit_status > 0:
    sys.exit(3)
