[R] markdown file and outputs (i.e. plots) for analysis of the Southern Region Pollinator Citizen Science Project. For more information, visit sixleggedaggie.com/pollinator-project

Files:
corrected_latnames.csv - A list of search-and-replace, manually created to correct the data set.
data.csv - the final corrected and completed data set. Columns described below:
	end_year: Year the observation was submitted.
	end_month: Month the observation was submitted
	end_time: The time (24-hr) the observation was submitted
	end_week: Week number (for 2019) the observation was submitted
	did: Device ID, as assigned by ona.io when a submission is made
	student_status: Multiple choice, whether the respondent was part of the elementary school program
	firstobs: Binary (Yes/No) whether observation was the first one made by that user within the last couple hours. When "No" was selected, we copied information was the same User ID for that instance into the above cells (such as temperature, coordinates, etc.). This was designed to reduce the amount of information users had to repeat if/when they were making multiple observations in a short window of time.
	loclat: Latitude of observation
	loclong: Longitude of observation
	time: Full date/time
	temp: Temperature at time of observation, in F
	latname: User inputted latin name of plant species being observed. Do not suggest using this analysis, as it could be erroneous. See later columns for plant taxonomy information
	cultivar: plant cultivar being observed
	bloom: A percentage rating of how much of the plant was in bloom.
		0 = Less than 25%
		1 = Between 25 - 50%
		2 = Between 51 - 75%
		3 = More than 75%
	colors (red - whitepink): The next several columns refer to the colors of the flowers. Users could check multiple colors, as flowers could potentially belong to more than one category. Responses are recorded as “1” (that color is present) or “0” (color is not present). The colors are self-explanatory, with a few exceptions:
		blue = blue/cyan
		purp = purple/indigo/violet
		whitepink = white/pink
	Pollinators (largebee - butterflies): Number of different pollinators observed within 60 seconds. See pollinator project website for more information on pollinator classification used for this project.
	feedback: open-ended feedback field for users to add information
	state: State in which the observation was made, based on coordinate information
	County: County in which the observation was made, based on coordinate information
	Family: Plant family observed, after checking latname against theplantlist.org.
	New.Genus: Plant genus, after checking latname against theplantlist.org
	New.Species: Plant species, after checking latname against theplantlist.org
	Authority: Plant species authority, after checking latname against theplantlist.org
	Taxonomic.status: If "Accepted" or "Synonym", then Family - Authority are reliable. Otherwise, the plant taxonomy to species level is unreliable.
	genus_and_family: If "Yes", then the Family and Genus of the plant taxonomic information is reliable. If "No", then none of the plant taxonomy information is reliable and should be omitted when modeling/summarizing information to plant types.

 