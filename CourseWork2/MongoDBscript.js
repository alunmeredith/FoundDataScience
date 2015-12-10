// Importing data
mongoimport "CW2-Dataset.csv" --type "CSV" --headerline --db --upsertDataScienceCoursework --collection microBlogData
use DataScienceCoursework

// How many unique users are there
db.microBlogData.distinct( "id_ member" ).length
// 119231

// How many tweets did the top 10% of users publish
db.microBlogData.aggregate(
	{
		$group: {_id : "$id_member", total : { $sum : 1}}
	},
	{
		$sort: {total : -1}
	},
	{
		$limit: 10
	},
	{
		$group: { _id : null, top10 : { $sum: "$total"}}
	}
)
// 32344

// 3. What was the earliest and latest date that a paper was published. 
db.microBlogData.aggregate(
	[
		{
			$group: 
			{ 
				_id: null, 
				early: { $min: "$timestamp"}
			}
		}
	]
)
