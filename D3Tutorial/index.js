jQuery(document).ready(function() {
var svg;
 var setup = function() { svg = d3.select('body').append('svg'); };
$.get('/tweets.json').then(function(tweets) {
 console.log('tweets > ', tweets.tweets);
 setup();
 window.tweets = tweets.tweets;
 }).fail(function(data) {
 console.error('error loading data ', data);
 });
});