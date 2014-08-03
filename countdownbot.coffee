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
	solution = solve tweet
	console.log solution
	# tweet the status as a response to the given tweet...

solve = (tweet) ->
	# given a tweet content along with the nick mention 
	# return the solution to the numbers game.
	# like removing everything except for the list of numbers
	# ideally tweet = @nick 876 2 5 8 25 7 50
	numbers = extract tweet
	cmd = "numbersgame " + numbers
	return Shell.exec(cmd).output # it caliculates the solution synchronously for now.

extract = (string) ->
	# extract numbers list from the given string using regex r smething else
	return string

main = ->
	config = getConfig argv.config
	T = new Twit config.account
	me = config.nick
	stream = T.stream 'user', {track: me}, (err) ->
		console.log err if err
	stream.on 'tweet', (tweet) ->
		console.log "stream tweet event"
	stream.on 'warning', (err) ->
		console.log "stream warning event"
	stream.on 'connect', (err) ->
		console.log "stream connect event"
	stream.on 'reconnect', (err) ->
		console.log "stream reconnect event"
	stream.on 'connected', (err) ->
		console.log "stream connected event"
	stream.on 'error', (err) ->
		console.log "stream error event"


if require.main == module
	main()