import signal
import subprocess
import sys

def signal_handler(signal, frame):
    print("Interrupted")
    # Run shell script
    subprocess.call(["./SNAP.sh"])
    # Run another Python script
    subprocess.call(["python", "new_process.py"])
    sys.exit(0)

signal.signal(signal.SIGINT, signal_handler)

i = 0
while True:
    print(i)
    i+=1

