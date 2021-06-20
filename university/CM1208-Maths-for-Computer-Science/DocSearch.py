import time 
from functools import reduce
from collections import defaultdict
import numpy as np
import math

def calculate_angle(v1, v2):
	norm_v1 = np.linalg.norm(v1)
	norm_v2 = np.linalg.norm(v2)
	cos_theta = np.dot(v1, v2) / (norm_v1 * norm_v2)
	theta = math.degrees(math.acos(cos_theta))
	return theta

def bin_search(item, array):
	lowerBound = 1
	upperBound = len(array) - 1

	found = False
	while not found:
		if upperBound < lowerBound:
			return -1
		
		midPoint = math.floor(lowerBound + (upperBound - lowerBound) / 2)

		if array[midPoint] < item:
			lowerBound = midPoint + 1

		elif array[midPoint] > item:
			upperBound = midPoint - 1

		elif array[midPoint] == item:
			found = True
	return midPoint

with open('docs.txt','r') as file:
	#Building the dictionary
	dictionary = [word for word in file.read().split()]
	dictionary = list(set(dictionary))
	dictionary.sort()


with open('docs.txt', 'r') as file:
	textArray = file.readlines()
	textArray = [i.strip() for i in textArray] #Removing the '/n's

invIndex = defaultdict(set)
index = 1
for line in textArray:
	listOfWords = line.split()
	for word in listOfWords:
		invIndex[word].add(index)
	index+=1

invIndex = sorted(invIndex.items())
#textArray = [[list1], [list2]]
#enumerate(textArray) = [[0,(list1)], [1,(list2)]]
#dictionary = [word1, word2, word3]

#invIndexMap = map(lambda index :[x + 1 for x, data in enumerate(textArray) if dictionary[index[0]] in textArray[x].split()], enumerate(dictionary))
#invIndex = list(invIndexMap) THIS TAKES WAY TOO LONG
#print("--- %s seconds ---" % (time.time() - start_time))

with open('queries.txt', 'r') as file:
	initQueryArray = file.readlines()
	initQueryArray = [i.strip() for i in initQueryArray] #Removing the '/n's

finalQueryArray = []
for i in range(len(initQueryArray)):
	#Creating the query strings
	tempQueryArray = []
	for word in initQueryArray[i].split():
		if word in dictionary:
			tempQueryArray.append(word)
	finalQueryArray.append(tempQueryArray)

print(f"Words in dictionary: {len(dictionary)}")

for i in range(len(finalQueryArray)):
	relevant = []
	angleDict = {}
	print(f"Query: {initQueryArray[i]}")
	for word in finalQueryArray[i]:
		relevant.append(invIndex[bin_search(word, dictionary)][1])
		#This uses the dictionary to find the index and uses it with the inverted index
		
	relevant = reduce(lambda doc, nextDoc: nextDoc & doc, relevant)
	#Using the AND operation on the 1st and 2nd set, then the 2nd and 3rd and so on... 
	print(f"Relevant documents: {' '.join(str(element) for element in relevant)}")	
	
	queryVector = list(map(lambda x:1 if x in finalQueryArray[i] else 0, dictionary))
	for doc in relevant:
		docVector = []
		#I need to couunt the number of times a word appears 
		for word in dictionary:
			times = len([x for x in textArray[doc - 1].split() if word == x])
			docVector.append(times)
			angleDict[doc] = docVector
	

	finalList = []
	finalListKeys = []
	


	for vector in angleDict:
		finalAngle = calculate_angle(queryVector, angleDict[vector])
		finalList.append(finalAngle)
		finalListKeys.append(vector)
		#print(f"{key} {finalAngle}")

	
	finalList, finalListKeys = (list(t) for t in zip(*sorted(zip(finalList, finalListKeys))))

	for i in range(len(finalList)):
		print(f"{finalListKeys[i]} {round(finalList[i], 2)}")


