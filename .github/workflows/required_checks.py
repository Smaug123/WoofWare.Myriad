import os
import json
import sys

# Get the RESULTS environment variable
results_json = os.environ.get('RESULTS', '{}') or sys.exit(1)

# Parse the JSON string
try:
    results = json.loads(results_json)
except json.JSONDecodeError:
    print("Error: Unable to parse RESULTS as JSON")
    exit(1)

# Function to process each job
def process_job(job_name, job_data):
    status = job_data.get('outputs', {}).get('status')
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
    exit_status += process_job(job_name, job_data)

if exit_status > 0:
    sys.exit(1)
