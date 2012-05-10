package opennlp.scalabha.example

import com.codahale.jerkson.Json._

case class Tweet(text: String,retweeted: Boolean, created_at: String, geo: GeoInfo)

case class GeoInfo(`type`: String, coordinates: List[Double])

object ProcessTweets {

  def main(args: Array[String]) {
    
    val mode = args(0)

    if (mode == "json") {
      val tweet1 = parse[Tweet](ExampleTweets.simple)
      val tweet2 = parse[Tweet](ExampleTweets.geo)
      println(generate(tweet2))
      println(tweet2)
    }
    
    else {
      val tweets = io.Source.fromFile(args(1)).getLines.flatMap { line => 
	if (line.trim.length == 0 || line.startsWith("{\"delete")) None
	else Some(parse[Tweet](line))
      }
      //tweets.foreach(println)

      if (mode == "geo") {
	println("Latitude,Longitude")
	tweets.foreach { tweet =>
	  tweet.geo match {
    	    case null => 
    	      case GeoInfo(geoType, List(latitude, longitude)) => 
		println(latitude+","+longitude)
    	    case _ =>
	  }
	}
      }
      else if (mode == "chunk") {
	tweets
	  .flatMap(tweet => EnglishPipeline.getChunks(tweet.text))
	  .foreach(println)
      }
    }
  }
  
}

object ExampleTweets {

  val simple = """{"text":"But then again I don't have nun else to do","truncated":false,"retweeted":false,"geo":null,"retweet_count":0,"source":"\u003Ca href=\"http:\/\/twitter.com\/download\/android\" rel=\"nofollow\"\u003ETwitter for Android\u003C\/a\u003E","in_reply_to_status_id_str":null,"created_at":"Wed Apr 25 14:16:43 +0000 2012","in_reply_to_user_id_str":null,"id_str":"195154375906299904","coordinates":null,"in_reply_to_user_id":null,"favorited":false,"entities":{"hashtags":[],"urls":[],"user_mentions":[]},"contributors":null,"user":{"show_all_inline_media":false,"statuses_count":1456,"following":null,"profile_background_image_url_https":"https:\/\/si0.twimg.com\/images\/themes\/theme1\/bg.png","profile_sidebar_border_color":"C0DEED","screen_name":"money_makin_Ant","follow_request_sent":null,"verified":false,"listed_count":0,"profile_use_background_image":true,"time_zone":null,"description":"","profile_text_color":"333333","default_profile":true,"profile_background_image_url":"http:\/\/a0.twimg.com\/images\/themes\/theme1\/bg.png","created_at":"Wed Dec 14 23:49:05 +0000 2011","is_translator":false,"profile_link_color":"0084B4","followers_count":140,"url":null,"profile_image_url_https":"https:\/\/si0.twimg.com\/profile_images\/1796068788\/Jca8P4B9_normal","profile_image_url":"http:\/\/a0.twimg.com\/profile_images\/1796068788\/Jca8P4B9_normal","id_str":"437069152","protected":false,"contributors_enabled":false,"geo_enabled":false,"notifications":null,"profile_background_color":"C0DEED","name":"Anthony ","default_profile_image":false,"lang":"en","profile_background_tile":false,"friends_count":207,"location":"","id":437069152,"utc_offset":null,"favourites_count":1,"profile_sidebar_fill_color":"DDEEF6"},"id":195154375906299904,"place":null,"in_reply_to_screen_name":null,"in_reply_to_status_id":null}"""

  val geo = """{"text":"Is it bad that I love the feeling of superiority that I get when freshmen whine about registration? #seniorswag","truncated":false,"retweeted":false,"geo":{"type":"Point","coordinates":[30.2726019,-97.71570463]},"retweet_count":0,"source":"\u003Ca href=\"http:\/\/twitter.com\/download\/android\" rel=\"nofollow\"\u003ETwitter for Android\u003C\/a\u003E","in_reply_to_status_id_str":null,"created_at":"Wed Apr 25 15:17:22 +0000 2012","in_reply_to_user_id_str":null,"id_str":"195169636600385537","coordinates":{"type":"Point","coordinates":[-97.71570463,30.2726019]},"in_reply_to_user_id":null,"favorited":false,"entities":{"hashtags":[{"text":"seniorswag","indices":[100,111]}],"urls":[],"user_mentions":[]},"contributors":null,"user":{"show_all_inline_media":false,"statuses_count":1081,"following":null,"profile_background_image_url_https":"https:\/\/si0.twimg.com\/profile_background_images\/399757870\/bground.jpg","profile_sidebar_border_color":"C0DEED","screen_name":"AaaaaaaaronH","follow_request_sent":null,"verified":false,"listed_count":0,"profile_use_background_image":true,"time_zone":null,"description":"Follower of Christ. Student. Longhorn. Geek.","profile_text_color":"333333","default_profile":false,"profile_background_image_url":"http:\/\/a0.twimg.com\/profile_background_images\/399757870\/bground.jpg","created_at":"Fri Mar 20 23:45:26 +0000 2009","is_translator":false,"profile_link_color":"0084B4","followers_count":147,"url":null,"profile_image_url_https":"https:\/\/si0.twimg.com\/profile_images\/1215869665\/39573_1051246376619_1689540009_86727_5090645_n__1__normal.jpg","profile_image_url":"http:\/\/a0.twimg.com\/profile_images\/1215869665\/39573_1051246376619_1689540009_86727_5090645_n__1__normal.jpg","id_str":"25601430","protected":false,"contributors_enabled":false,"geo_enabled":true,"notifications":null,"profile_background_color":"C0DEED","name":"Aaron Hlavaty","default_profile_image":false,"lang":"en","profile_background_tile":false,"friends_count":208,"location":"","id":25601430,"utc_offset":null,"favourites_count":0,"profile_sidebar_fill_color":"DDEEF6"},"id":195169636600385537,"place":{"bounding_box":{"type":"Polygon","coordinates":[[[-97.938282,30.098659],[-97.56842,30.098659],[-97.56842,30.49685],[-97.938282,30.49685]]]},"country":"United States","place_type":"city","url":"http:\/\/api.twitter.com\/1\/geo\/id\/c3f37afa9efcf94b.json","attributes":{},"full_name":"Austin, TX","country_code":"US","name":"Austin","id":"c3f37afa9efcf94b"},"in_reply_to_screen_name":null,"in_reply_to_status_id":null}"""

}
