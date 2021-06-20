from matplotlib import pyplot as plt
import matplotlib.ticker as ticker
import numpy as np

mttf = 50000*60 
# Mean time to failure in minutes
mttr = [i for i in range(0, 24*24*31)]  
# Mean time to repair in minutes (0 mins - 31 days)
availability = [mttf/(mttf+i) for i in mttr]

plt.plot(mttr,availability)

plt.xlabel("Mean time to repair (minutes)")
plt.ylabel("Availability")
plt.title("Probability that the router is operational")
plt.show()

