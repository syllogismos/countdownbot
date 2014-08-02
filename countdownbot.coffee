Twit     = require 'twit'
Mustache = require 'mustache'
Shell    = require 'shelljs'
minimist = require 'minimist'


argv = minimist process.argv.slice(2), default:
  verbose: false
  config: './config.json'


getConfig = (path) ->
	if path[0] != '/' and path[0..1] != './'
		path = './' + path
	require(path)

config = getConfig argv.config
me = config.nick
T = new Twit config.account

tweet = (account, status) ->
	console.log status
	unless argv.noop
		T.post 'statuses/update', status: status, (err) ->
			console.log err if err

respondToTweet = (account, status, tweet) ->
	console.log status
	# tweet the status as a response to the given tweet...

solve = (tweet) ->
	# given a tweet content along with the nick mention 
	# return the solution to the numbers game.
	# like removing everything except for the list of numbers
	# ideally tweet = @nick 876 2 5 8 25 7 50
	numbers = cleanse(tweet)
	cmd = "numbersgame "++ numbers
	return Shell.exec(cmd).output # it caliculates the solution synchronously for now.

