from __future__ import with_statement
import SocketServer
import string
import sys
import threading
import time
import os
import glob
import logging
import traceback
import socket

def getGameSize():
	try:
		return string.atoi(sys.argv[3])
	except:
		return 8
	
def getNumFromGroup():
	try:
		return string.atoi(sys.argv[4])
	except:
		return 2

def getBotsDir():
	try:
		if os.access(sys.argv[1], os.R_OK):
			return sys.argv[1]
	except:
		return os.getcwd()

def getBotExpr():
	try:
		name, extention = sys.argv[2].split('.')
		name.index('*') and extension
		return sys.argv[2]
	except:
		return '*'
	
def getBotFileNames():
	try:
		return glob.glob(os.path.join(getBotsDir(), getBotExpr()))
	except:
		return []

logging.basicConfig(level=logging.DEBUG,
					format='%(asctime)s %(levelname)s - %(message)s',
					filename=os.path.join(getBotsDir(), 'QBotServer.log'),
					filemode='w')

class BotPool(object):
	def __init__(self, bots):
		self.lock = threading.RLock()
		self.bots = bots
		self.counter = 0
		self.botsWaitingResults = {}
		self.groupIter = self.botsWaitingResults.keys().__iter__()		
	def getNBots(self, n):
		with self.lock:
			if not self.bots:
				return self.__retryWaiting()
			botGroup = self.bots[:n]
			del self.bots[:n]
			token = self.__getToken()
			self.botsWaitingResults[token] = botGroup
			logging.debug("returning %d bots with token %s" % (len(botGroup), token))
			return [token, botGroup]
	def __retryWaiting(self):
		with self.lock:
			if not self.botsWaitingResults:
				raise StopIteration
			try:
				token = self.groupIter.next()
				logging.debug("retrying group with token %s" % token)
				return [token, self.botsWaitingResults[token]]
			except (StopIteration, KeyError):
				self.groupIter = self.botsWaitingResults.keys().__iter__()
				return self.__retryWaiting()
	def removeFromPool(self, token):
		with self.lock:
			if token in self.botsWaitingResults:
				logging.debug("removing group from pool with token %s" % token)
				del self.botsWaitingResults[token]
			else:
				raise ValueError
	def __getToken(self):
		token = ("%f%d" %(time.time(), self.counter))
		self.counter += 1
		return token

class TournamentOrganizer(object):
	def __init__(self):
		self.mutex = threading.RLock()
		self.fileNameMap = {}
		for fileName in getBotFileNames():
			name = os.path.split(fileName)[-1].split('.')[0]
			self.fileNameMap[name] = fileName
		self.tierdResults = [{}]
		self.botPool = BotPool(self.fileNameMap.keys())
		self.gameSize = getGameSize()
		self.numFromGroup = getNumFromGroup()
		self.lastTier = len(self.fileNameMap) <= self.gameSize
		logging.info("starting tournament with %d bots, %d bot games and %d bots advancing to the next tier"
					% (len(self.fileNameMap), self.gameSize, self.numFromGroup))
	def __iter__(self):
		return self
	def next(self):
		with self.mutex:
			try:
				return self.botPool.getNBots(self.gameSize)
			except StopIteration:
				if self.fileNameMap :
					self.lastTier = len(self.fileNameMap) <= self.gameSize
					self.botPool = BotPool(self.fileNameMap.keys())
					self.tierdResults.append({})
					logging.info("starting new tier with %d bots" % len(self.fileNameMap))
					return self.next()
				else:
					raise StopIteration
	def registerResults(self, token, results):
		try:
			results.sort(key = lambda x: string.atof(x[0]), reverse = True)
			for score, bot in results:
				self.fileNameMap[bot] #raise a KeyError if the bot isn't known
			if len(results) != self.gameSize:
				raise ValueError
			self.botPool.removeFromPool(token)
			with self.mutex:
				index = 0 if self.lastTier else self.numFromGroup
				for score, bot in results[index:]:
					logging.debug("%s returned with fitness %s" % (bot, score))
					del self.fileNameMap[bot]
					self.tierdResults[-1][bot] = score
		except KeyError:
			logging.warning("unrecognized bot name or previously posted result")
		except ValueError:
			logging.warning("incorrect number of bots or unrecognized token")
	def getFileName(self, bot):
		return self.fileNameMap[bot]
	def getLispResults(self):
		results = []
		for tier in self.tierdResults:
			tierTotal = sum(map(string.atof, tier.itervalues()))
			tierData = ' '.join(['(%s . "%s")' % (value, key) for key, value in tier.iteritems()])
			results.append("(%f (%s))" %(tierTotal, tierData))
		return '(%s)' % (' '.join(results))


class QBotEvoRequestHandler(SocketServer.StreamRequestHandler):
	
	def handle(self):
		logging.debug("incomming request")
		try:
			command = self.rfile.next()
			if command == "GETBOTS\n":
				self.__getBots()
			elif command == "POSTRESULTS\n":
				self.__postResults()
		except (IOError, StopIteration, socket.error, socket.timeout), e:
			logging.error("communication or protocol error: %s" % str(e))
		except:
			logging.critical(traceback.format_exc())
		finally:
			logging.debug("finished processing request")
			self.rfile.close()
			self.wfile.close()
	def __getBots(self):
		logging.debug("sending bots")
		try:
			token, bots = server.tournamentOrganizer.next()
			self.wfile.write("%s\n" %(token))
			for bot in bots:
				self.wfile.write("STARTBOT %s\n" %(bot))
				f = open(self.server.tournamentOrganizer.getFileName(bot), "r")
				for line in f:
					self.wfile.write(line)
				self.wfile.write("ENDBOT %s\n" %(bot))
		except StopIteration:
			logging.debug("finished serving bots")
			self.server.done = True
	def __postResults(self):
		logging.debug("recieving results")
		token = self.rfile.next().strip()
		results = []
		while True:
			try:
				results.append(self.rfile.next().rstrip().split(' '))
			except StopIteration:
				break
		self.server.tournamentOrganizer.registerResults(token, results)
		
class QBotServer(SocketServer.ThreadingTCPServer):
	# allow the socket to bind the same port again after computing the next
	# generation and restarting
	allow_reuse_address = True
	
	def __init__(self, server_address, RequestHandlerClass):
		SocketServer.ThreadingTCPServer.__init__(self, server_address, RequestHandlerClass)
		self.done = False
		self.tournamentOrganizer = TournamentOrganizer()
	def handle_error(self, request, client_address):
		logging.error("error processing request from: %s" % str(client_address))
		logging.error(traceback.format_exc())
	def serveQBots(self):
		while not self.done:
			self.handle_request()
		self.server_close()
		return self.tournamentOrganizer.getLispResults()

try:
	server = QBotServer(('', 28000), QBotEvoRequestHandler)
	result = server.serveQBots()
	sys.stdout.write(result)
except:
	logging.critical(traceback.format_exc())
