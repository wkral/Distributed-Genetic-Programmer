#!/usr/bin/env python

import re
import sys

headerPath = sys.argv[1]
bodyPath = sys.argv[2]
footerPath = sys.argv[3]
outputPath = sys.argv[4]

FLOAT = 'gpFLOAT'
FLOATstring = '" float "'
INT = 'gpINT'
INTstring = '" int "'

def transformScaffolding(scaffoldingPath):

	f = open(scaffoldingPath, 'r')
	
	body = ''
	for line in f:

		line = line.replace('\\', '\\\\').replace('"', '\\"')
		if re.search(FLOAT, line):
			line = line.replace(FLOAT, FLOATstring)
		elif re.search(INT, line):
			line =  line.replace(INT, INTstring)
		body = body + line

	f.close()
	return body


out = open(outputPath, 'w')
for line in open(headerPath, 'r'):
	out.write(line)
out.write(transformScaffolding(bodyPath))
for line in open(footerPath, 'r'):
	out.write(line)
out.close()

