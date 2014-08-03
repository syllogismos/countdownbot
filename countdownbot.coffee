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

tweet = (T, status) ->
	console.log status
	unless argv.noop
		T.post 'statuses/update', status: status, (err) ->
			console.log err if err

respondToTweet = (T, status, tweet) ->
	console.log "responding to the tweet " + tweet.text
	console.log "response is " + status
	if status != "-1"
		T.post 'statuses/update', {status: status, in_reply_to_status_id:tweet.id_str}, (err) -> 
			console.log "tweet posted"


process = (tweet) ->
	# given a tweet content along with the nick mention 
	# return the solution to the numbers game.
	# like removing everything except for the list of numbers
	# ideally tweet = @nick 876 2 5 8 25 7 50
	return "-1" if tweet.text.indexOf "numbersgame" == -1
	numbers = extract tweet.text
	cmd = "numbersgame " + numbers
	response = "@" + tweet.screen_name + " " + Shell.exec(cmd).output # we are using Shell.exec synchronously for now.
	return response

extract = (string) ->
	# extract numbers list from the given string using regex or something else
	return "984 10 7 50 3 1 8 75"

main = ->
	config = getConfig argv.config
	T = new Twit config.account
	me = config.nick
	stream = T.stream 'user', {track: me}, (err) ->
		console.log err if err
	stream.on 'tweet', (tweet) ->
		console.log "stream tweet event"
		response = process tweet # response must start with the twitter handle of the author of the tweet we are responding to
		respondToTweet T, response, tweet
	stream.on 'warning', (err) ->
		console.log "stream warning event"
	stream.on 'connect', (err) ->
		console.log "stream connect event"
	stream.on 'reconnect', (err) ->
		console.log "stream reconnect event"
	stream.on 'connected', (err) ->
		console.log "stream connected event"
		#console.log "posting test reply tweet"
		#T.post 'statuses/update', {status: "@2abstract4me lel", in_reply_to_status_id: "495423556331642880"}, (err) ->
		#	console.log "error in posting the reply tweet"
	stream.on 'error', (err) ->
		console.log "stream error event"


if require.main == module
	main()