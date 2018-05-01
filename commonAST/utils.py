import refMaps
import context
import match 

def hasTags(node):
	return "tags" in node 

def matchNodes(node1, node2, conf):
	#print("matching nodes", node1["tags"], "and", node2["tags"])
	node1["matched"] = True
	node2["matched"] = True
	node1["match"] = match.Match(node2, conf)
	node2["match"] = match.Match(node1, conf)

def unMatchNode(node):
	node["match"] = None
	node["matched"] = False

def findMultTags(tags):
	multTags = []
	for tag in tags:
		if tag == None: continue
		if tag[0] == "+" and len(tag) > 1:
			multTags.append(tag[1:])
	return multTags

def findDenyTags(tags):
	denyTags = []
	for tag in tags:
		if tag == None: continue
		if tag[0] == "!":
			denyTags.append(tag[1:])

	return denyTags


'''
Given two nodes, check if any tags are equal
'''
def tagsMatch(node1, node2, parent1, parent2, lang, rec, index1=0, index2=0):
	tags1 = node1["tags"]
	tags2 = node2["tags"]
	
	multCount1 = 0 
	multCount2 = 0

	#check if the tags are naively equal
	for tag in tags1:
		if tag == "\*": return 1
		for tag_ in tags2:
			if tag_ == "\*": return 1
			if tag.lower() == tag_.lower(): #and len(mult) == 0:
				return calculateConfidence(node1, node2, 1, lang, index1, index2)
 
	if rec: 
		confidence1 = equalTags(node1, node2, parent1, parent2, lang, index1, index2) 
		if not confidence1 == -1:
			return confidence1

		return equalTags(node2, node1, parent2, parent1, lang, index2, index2)

	else:
		confidence2 = equalTagsNotRec(node1, node2, parent1, parent2, lang) 
		if not confidence2 == -1:
			return confidence2

		return equalTagsNotRec(node2, node1, parent2, parent1, lang)


def cntxtInsensitiveCheck(eqObj, tags2):
	tags2 = [tag.lower() for tag in tags2]

	for tag_ in eqObj.tags:
		if not tag_.lower() in tags2:
			return False
	return True

def confidenceOfMatch(node, node2, parent, parent2, lang, rec, index1, index2):
	if hasTags(node) and hasTags(node2): #and not node2["matched"]:
		confidence1 = tagsMatch(node, node2, parent, parent2, lang, rec, index1, index2)
		if not confidence1 == -1:
			return confidence1
		elif not hasTags(node) and not hasTags(node2) and typesMatch(node, node2) and datasMatch(node, node2): return 1

	return -1


def getAllPotentialMatches(node, t2Nodes,lang, index1, rec=True):
	potentialMatches = []
	unmatchedNodes = []

	index2 = 0
	for node2 in t2Nodes:
		if additionalStructure(node2, lang):
			if not "children" in node2: continue
			editChildren(node2)
			for _child in node2["children"]:
				if not additionalDetail(node2, lang) and not _child["matched"]:
					unmatchedNodes.append([_child, index2]);
				if not additionalDetail(node2, lang): index2 += 1

		elif not node2["matched"] and not additionalDetail(node2, lang):
				unmatchedNodes.append([node2, index2])
				index2 += 1
		elif not additionalDetail(node2, lang):
				index2 += 1


	#first try ones with the same index:
	for node2 in unmatchedNodes:
		if node2[1] == index1:
			confidence = confidenceOfMatch(node, node2[0], node["parent"], node2[0]["parent"], lang, rec, index1, node2[1]) 
			if confidence == 1:
				return [match.Match(node2[0], confidence, node2[1])]


	for node2 in unmatchedNodes:
		confidence = confidenceOfMatch(node, node2[0], node["parent"], node2[0]["parent"], lang, rec, index1, node2[1]) 

		if not confidence == -1:
			potentialMatches.append(match.Match(node2[0], confidence, node2[1]))
			#print("adding confidence of matching:", node["tags"], ":", index1, "with", node2[0]["tags"],":", node2[1], "conf: ", confidence)

		if confidence == 1:
			return [match.Match(node2[0], confidence, node2[1])]
				
	return potentialMatches


def additionalDetail(node, lang):
	if not "tags" in node: return True
	for tag in node["tags"]:
		if tag.lower() in refMaps.adlDetailMap:
			for cntxt in refMaps.adlDetailMap[tag.lower()]:
				if cntxt == createContext(node,lang): 
					return True

	return False

'''
Given a node and its parent, see if it exists in the structEql map
'''
def additionalStructure(node,lang):
	if not hasTags(node): return False
	for tag in node["tags"]:
		if tag.lower() in refMaps.adlStructMap:
			for cntxt in refMaps.adlStructMap[tag.lower()]:
				if createContext(node,lang) == cntxt:
					return True

	return False



def editChildren(node):
	for child in node["children"]:
		if not "parent" in child: child["parent"] = node
		if not "matched" in child: child["matched"] = False		

def getBestMatch(potentialMatches):
	#print("potentialMatches:")

	bestConfValue = -1
	bestMatch = None
	for match in potentialMatches:
		#print(match.index, match.node["tags"], match.confidence)
		if match.confidence > bestConfValue:
			bestMatch = match
			bestConfValue = match.confidence

	return bestMatch

def getNumMatchableChildren(children, lang):
	num = 0
	for child in children:
		if additionalStructure(child, lang) and "children" in child:
			num += getNumMatchableChildren(child["children"], lang)
		elif not additionalDetail(child, lang):
			num += 1
	return num

def calculateConfidence(node1, node2, level, lang, index1, index2):
	numMatch = 0 
	if "children" in node1 and "children" in node2:
		editChildren(node1)
		editChildren(node2)

		'''	
		for child in node1["children"]:
			child["parent"] = node1
			if len(getAllPotentialMatches(child, node2["children"], lang, index1, False)) >= 1:
				numMatch += 1
		'''

		'''
		numUnmatchable = 0
		unmatchable = (_child for _child in node2["children"] if additionalStructure(_child,lang) or additionalDetail(_child, lang))
		for unmatch in unmatchable:
			numUnmatchable += 1

		numUnmatchable1 = 0
		unmatchable1 = (_child for _child in node1["children"] if additionalStructure(_child,lang) or additionalDetail(_child, lang))
		for unmatch in unmatchable1:
			numUnmatchable1 += 1
		'''


		numChildren1 = getNumMatchableChildren(node1["children"], lang)
		numChildren2 = getNumMatchableChildren(node2["children"], lang)

		#totalChildren = (len(node2["children"]) - numUnmatchable)+ (len(node1["children"]) - numUnmatchable1)
		totalChildren = abs(numChildren1 - numChildren2)
		#if totalChildren == 0: return 1

		#part1 = number of children
		if numChildren1 + numChildren2 == 0: part1 = 1
		else: part1 = 1 - (totalChildren / (numChildren1 + numChildren2))

		#if (len(node2["children"]) - numUnmatchable) - (len(node1["children"]) - numUnmatchable1) == 0: part1 = 1  #.5
		#else: part1 = 0


		#part2 = matches in the children
		#part2 = .33
		#part2 = (numMatch / totalChildren)  / 3

		#part3 = differences in indicies
		#part3 = (1 - abs(index1 - index2)) / 2

		#return  part1 + part2 + part3
		return  part1 #+ part3


	if not "children" in node1 and not "children" in node2: return 1
	elif "children" in node1 and not "children" in node2:
		numChildren1 = getNumMatchableChildren(node1["children"], lang)
		if numChildren1 == 0: return 1
		else: return 0
	elif "children"  in node2 and not "children" in node1:
		numChildren2 = getNumMatchableChildren(node2["children"], lang)
		if numChildren2 == 0: return 1
		else: return 0


'''
Checks if two tags are equal using the equality map
'''
def equalTags(node1, node2, parent1, parent2, lang, index1, index2):
	tags1 = node1["tags"]
	tags2 = node2["tags"]

	#if it is a key in the equality map with an empty context
	for tag in tags1:
		if tag.lower() in refMaps.tagEqlMap:
			for eqObj in refMaps.tagEqlMap[tag.lower()]:
				if eqObj.context == context.Context(lang) and cntxtInsensitiveCheck(eqObj, tags2): 
					return calculateConfidence(node1, node2, 1, lang, index1, index2)
				elif cntxtInsensitiveCheck(eqObj, tags2) and contextSensitiveCheck(eqObj, node1, lang):  
					return calculateConfidence(node1, node2, 1,lang, index1, index2)
	return -1

def equalTagsNotRec(node1, node2, parent1, parent2, lang):
	tags1 = node1["tags"]
	tags2 = node2["tags"]

	#if it is a key in the equality map with an empty context
	for tag in tags1:
		if tag.lower() in refMaps.tagEqlMap:
			for eqObj in refMaps.tagEqlMap[tag.lower()]:
				if eqObj.context == context.Context(lang) and cntxtInsensitiveCheck(eqObj, tags2): return 1
				elif cntxtInsensitiveCheck(eqObj, tags2) and contextSensitiveCheck(eqObj, node1, lang): return 1
	return -1


def createContext(node,lang):
	lookaheadTags = None
	siblingTags = None
	parentTags = None
	gpTags = None

	if "children" in node:
		lookaheadTags = []
		for child in node["children"]:
			if "tags" in child:
				lookaheadTags += child["tags"]

	if not lookaheadTags == None and len(lookaheadTags) == 0:
		lookaheadTags = None

	if "parent" in node and "children" in node["parent"]:
		siblingTags = []
		for child in node["parent"]["children"]:
			if "tags" in child and not tagsMatch(child, node, node["parent"], node["parent"], lang, False, 0, 0):
				siblingTags += child["tags"]

	if not siblingTags == None and len(siblingTags) == 0:
		siblingTags = None

	if "parent" in node and "tags" in node["parent"]:
		parentTags = node["parent"]["tags"]


	if "parent" in node and "parent" in node["parent"] and "tags" in node["parent"]["parent"]:
		gpTags = node["parent"]["parent"]["tags"]

	return context.Context(lang, lookaheadTags, siblingTags, parentTags, gpTags)

def contextSensitiveCheck(eqObj, node1, lang):
	return eqObj.context == createContext(node1, lang)
