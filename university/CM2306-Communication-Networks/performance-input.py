import matplotlib
import numpy as np

time_to_process = int(input("Time taken to process a job (ms)"))
hits_per_second = int(input("Hits per second"))

processed_per_second = 1000/time_to_process

#1000 ms in 1 second
#10ms would result in 100 processed_per_second.

if processed_per_second == hits_per_second:
    utilisation = 0.99
else:
    utilisation = hits_per_second/processed_per_second

response_time = (utilisation * processed_per_second)/(2 * (1 - utilisation))

print(response_time)
