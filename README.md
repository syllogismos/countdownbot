
install the required npm modules

>> NumbersGame 983 10 8 4 100 7 5

>> One possible solution for 983 is: (10*(4+(100-5)))-7

basic idea is that when someone mentions the bot, with the target number and the list of numbers like below and it will reply with the solution

@twitterbot numbersgame 983 10 8 4 100 7 5

and it will reply to the above tweet


Sample tweet object

{ created_at: 'Sun Aug 03 15:04:16 +0000 2014',
  id: 495948270116413440,
  id_str: '495948270116413441',
  text: 'numbersbot @wikieditsbot',
  source: '<a href="http://twitter.com" rel="nofollow">Twitter Web Client</a>',
  truncated: false,
  in_reply_to_status_id: null,
  in_reply_to_status_id_str: null,
  in_reply_to_user_id: null,
  in_reply_to_user_id_str: null,
  in_reply_to_screen_name: null,
  user:
   { id: 2360845790,
     id_str: '2360845790',
     name: 'syllogismos',
     screen_name: '2abstract4me',
     location: '',
     url: null,
     description: 'Aalways look on the bright siide of life... tutu tutootutootu
tu \r\n\r\nhttp://i.imgur.com/lMrr3YC.gif',
     protected: false,
     followers_count: 6,
     friends_count: 34,
     listed_count: 0,
     created_at: 'Tue Feb 25 10:06:04 +0000 2014',
     favourites_count: 70,
     utc_offset: -25200,
     time_zone: 'Pacific Time (US & Canada)',
     geo_enabled: false,
     verified: false,
     statuses_count: 383,
     lang: 'en',
     contributors_enabled: false,
     is_translator: false,
     is_translation_enabled: false,
     profile_background_color: 'EDECE9',
     profile_background_image_url: 'http://pbs.twimg.com/profile_background_imag
es/450322687378726912/j1mcV2M6.jpeg',
     profile_background_image_url_https: 'https://pbs.twimg.com/profile_backgrou
nd_images/450322687378726912/j1mcV2M6.jpeg',
     profile_background_tile: true,
     profile_image_url: 'http://pbs.twimg.com/profile_images/478801610718466048/
Nxb05zUv_normal.jpeg',
     profile_image_url_https: 'https://pbs.twimg.com/profile_images/478801610718
466048/Nxb05zUv_normal.jpeg',
     profile_banner_url: 'https://pbs.twimg.com/profile_banners/2360845790/14012
98696',
     profile_link_color: '088253',
     profile_sidebar_border_color: 'FFFFFF',
     profile_sidebar_fill_color: 'DDEEF6',
     profile_text_color: '333333',
     profile_use_background_image: true,
     default_profile: false,
     default_profile_image: false,
     following: null,
     follow_request_sent: null,
     notifications: null },
  geo: null,
  coordinates: null,
  place: null,
  contributors: null,
  retweet_count: 0,
  favorite_count: 0,
  entities:
   { hashtags: [],
     symbols: [],
     urls: [],
     user_mentions: [ [Object] ] },
  favorited: false,
  retweeted: false,
  filter_level: 'medium',
  lang: 'en' }

