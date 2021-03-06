/****** Script for SelectTopNRows command from SSMS  ******/
 
	-- Selects all fields from the comBined Taxi table.
	SELECT TOP 100 * FROM [DCU Machine Learning].[dbo].[TaxiData]
  
	-- Due to appending all 6 files in the command prompt, header information was included within the data. This code removes it.
	DELETE FROM [DCU Machine Learning].[dbo].[TaxiData]
	where [pickup_datetime] = 'pickup_datetime'

	---- Deletes user defined columns, incase they need to be re-created for adjustments.
	-- ALTER TABLE [DCU Machine Learning].[dbo].[TaxiData]
	-- DROP COLUMN PickUp_LatLong, DropOff_LatLong

	---- Creates two additional columns which will hold concatenated data.
	-- ALTER TABLE [DCU Machine Learning].[dbo].[TaxiData]
	-- ADD PickUp_LatLong VARCHAR(100),
	--DropOff_LatLong VARCHAR(100)

	---- Updates created columns with concatenated data.
	-- UPDATE [DCU Machine Learning].[dbo].[TaxiData]
	-- SET PickUp_LatLong = CAST(ROUND(CAST(pickup_latitude AS FLOAT),4) AS VARCHAR) + ':' + CAST(ROUND(CAST(pickup_longitude AS FLOAT),4) AS VARCHAR),
	--DropOff_LatLong = CAST(ROUND(CAST(dropoff_latitude AS FLOAT),4) AS VARCHAR) + ':' + CAST(ROUND(CAST(dropoff_longitude AS FLOAT),4) AS VARCHAR)

	---- Code to test on a sample basis as it is faster to run.
	-- SELECT CAST(ROUND(CAST(pickup_latitude AS FLOAT),4) AS VARCHAR) + ':' + CAST(ROUND(CAST(pickup_longitude AS FLOAT),4) AS VARCHAR) 
	-- FROM [DCU Machine Learning].[dbo].[TaxiData]
	-- WHERE [pickup_datetime] = '2016-01-01 00:00:00'



	-- ****************************************
	-- ********** Distinct Locations **********
	-- ****************************************

	-- Get distinct pcikup locations.
	SELECT distinct round([pickup_latitude],4) as Pickup_Latitude,
		round([pickup_longitude],4) as Pickup_Longitude
		INTO  [DCU Machine Learning].[dbo].[Pickup_Summary]
	FROM [DCU Machine Learning].[dbo].[TaxiData]

	-- Get distint dropoff locations.
	SELECT distinct round([dropoff_latitude],4) as Dropoff_Latitude,
		round([dropoff_longitude],4) as Dropoff_Longitude
		INTO  [DCU Machine Learning].[dbo].[Dropoff_Summary]
	FROM [DCU Machine Learning].[dbo].[TaxiData]

	-- ****************************************
	-- ********* Formating Date Field *********
	-- ****************************************

	DROP TABLE [DCU Machine Learning].[dbo].[DistinctDate]
	-- Get distinct Date.
	SELECT DISTINCT SUBSTRING(pickup_datetime,1,10) AS pickupDate
		INTO [DCU Machine Learning].[dbo].[DistinctDate]
	FROM [DCU Machine Learning].[dbo].[TaxiData]

	-- Adds new columns to table.
	ALTER TABLE [DCU Machine Learning].[dbo].[DistinctDate]
	ADD Week_Day VARCHAR(9),
		Month VARCHAR(8),
		DAY VARCHAR(2)
  
	-- Updates new columns.
	UPDATE [DCU Machine Learning].[dbo].[DistinctDate]
	SET Week_Day = DATENAME(weekday, pickupDate),
		Month = DATENAME(month, pickupDate), 
		DAY = DATENAME(day, pickupDate)


	-- ****************************************
	-- ********* Formating Time Field *********
	-- ****************************************

	DROP TABLE [DCU Machine Learning].[dbo].[DistinctTime]
	-- Get distinct Time.
	SELECT DISTINCT SUBSTRING(pickup_datetime,12,5) AS pickUpTime
		INTO [DCU Machine Learning].[dbo].[DistinctTime]
	FROM [DCU Machine Learning].[dbo].[TaxiData]

	-- Adds new columns to table.
	ALTER TABLE [DCU Machine Learning].[dbo].[DistinctTime]
	ADD TimeInterval VARCHAR(10) 

	-- Updates new columns.
	UPDATE [DCU Machine Learning].[dbo].[DistinctTime]
	SET TimeInterval = CASE WHEN  CAST(SUBSTRING(pickUpTime,4,2) AS INT) <= 30 THEN SUBSTRING(pickUpTime,1,3) + '00' ELSE SUBSTRING(pickUpTime,1,3) + '30' END


	-- ****************************************
	-- ****** Join Location, Time & Date ******
	-- ****************************************
	DROP TABLE [DCU Machine Learning].[dbo].[TaxiData_Updated]
	Select a.*,
		case when b.GeohashP5 in ('dr72h', 'dr72j', 'dr72n','dr5ru', 'dr5rv', 'dr5ry','dr5rs','dr5rt', 'dr5rw', 'dr72p', 'dr5rz', 'dr5rx')
		then b.GeohashP6 else b.GeohashP5 end AS pickup_Geohash,
		case when c.GeohashP5 in ('dr72h', 'dr72j', 'dr72n','dr5ru', 'dr5rv', 'dr5ry','dr5rs','dr5rt', 'dr5rw', 'dr72p', 'dr5rz', 'dr5rx')
		then c.GeohashP6 else c.GeohashP5 end AS dropoff_Geohash,
		d.pickupDate,
		d.Week_Day,
		d.Month,
		d.DAY,
		e.pickUpTime,
		e.TimeInterval
	
	INTO [DCU Machine Learning].[dbo].[TaxiData_Updated]
	FROM [DCU Machine Learning].[dbo].[TaxiData] a
	LEFT JOIN [DCU Machine Learning].[dbo].[TaxiPickupsRetain] b
		ON ROUND(a.pickup_latitude,4) = b.[Taxi Lat] and ROUND(a.pickup_longitude,4) = b.[Taxi Long]
	LEFT JOIN [DCU Machine Learning].[dbo].[TaxiDropOffsRetain] c
		ON ROUND(a.dropoff_latitude,4) = c.Dropoff_Latitude AND ROUND(a.dropoff_longitude,4) = c.Dropoff_Longitude
	LEFT JOIN [DCU Machine Learning].[dbo].[DistinctDate] d
		ON SUBSTRING(a.pickup_datetime,1,10) = d.pickupDate
	LEFT JOIN [DCU Machine Learning].[dbo].[DistinctTime] e
		ON SUBSTRING(a.pickup_datetime,12,5) = e.pickUpTime


	--SELECT * FROM [DCU Machine Learning].[dbo].[TaxiData_Updated]
	--where pickup_Geohash <> dropoff_Geohash

	-- ****************************************
	-- ***** Summarize on pickup location *****
	-- ****************************************
	DROP TABLE [DCU Machine Learning].[dbo].[TaxiData_Summarized]
	SELECT pickupDate, Week_Day, [Month], [DAY], TimeInterval, pickup_Geohash, 
		count(*) as Num_Jrnys, sum(cast(passenger_count as FLOAT)) as total_passenger_count,
		ROUND(sum(CAST(fare_amount AS FLOAT)),4) as total_fare
	INTO [DCU Machine Learning].[dbo].[TaxiData_Summarized]
	FROM [DCU Machine Learning].[dbo].[TaxiData_Updated]
	GROUP BY pickupDate, Week_Day, [Month], [DAY], TimeInterval, pickup_Geohash;

	
	-- Include lagged pickup densities. For example at each geohash, the pickup density at the prior week,  day, hour and half hour
	DROP TABLE [DCU Machine Learning].[dbo].[TaxiData_Summarized_With_Lags]
	select a.pickupDate, a.Week_Day, a.month, a.DAY, a.TimeInterval,a.pickup_Geohash, a.Num_Jrnys, a.total_passenger_count, a.total_fare,
	isnull(b.Num_Jrnys,0) as Num_Jrnys_Prev_Day,
	isnull(c.Num_Jrnys,0) as Num_Jrnys_Prev_Week,
	isnull(d.Num_Jrnys,0) as Num_Jrnys_Prev_Hour,
	isnull(e.Num_Jrnys,0) as Num_Jrnys_Prev_HalfHour
	into [DCU Machine Learning].[dbo].[TaxiData_Summarized_With_Lags]
	from 
	(select *, cast((pickupdate  + ' '+ timeinterval)as datetime) as CurrentDateTime,
	dateadd(dd, -1,cast((pickupdate  + ' '+ timeinterval)as datetime)) as MinusDay,
	dateadd(dd, -7,cast((pickupdate  + ' '+ timeinterval)as datetime)) as MinusWeek,
	dateadd(HH, -1,cast((pickupdate  + ' '+ timeinterval)as datetime)) as MinusHour,
	dateadd(MINUTE, -30,cast((pickupdate  + ' '+ timeinterval)as datetime)) as MinusHalfHour
	from [DCU Machine Learning].[dbo].[TaxiData_Summarized]) a
	left join [DCU Machine Learning].[dbo].[TaxiData_Summarized] b
	on a.MinusDay = cast((b.pickupdate  + ' '+ b.timeinterval)as datetime) and a.pickup_Geohash = b.pickup_Geohash
	left join [DCU Machine Learning].[dbo].[TaxiData_Summarized] c
	on a.MinusWeek = cast((c.pickupdate  + ' '+ c.timeinterval)as datetime) and a.pickup_Geohash = c.pickup_Geohash
	left join [DCU Machine Learning].[dbo].[TaxiData_Summarized] d
	on a.MinusHour = cast((d.pickupdate  + ' '+ d.timeinterval)as datetime) and a.pickup_Geohash = d.pickup_Geohash
	left join [DCU Machine Learning].[dbo].[TaxiData_Summarized] e
	on a.MinusHalfHour = cast((e.pickupdate  + ' '+ e.timeinterval)as datetime) and a.pickup_Geohash = e.pickup_Geohash


	-- ****************************************
	-- ********* Add in weather data *********
	-- ****************************************

	Drop Table [DCU Machine Learning].[dbo].[Summarized_inc_weather_Final]
	SELECT distinct a.[pickupDate]
      ,[Week_Day]
      ,[Month]
      ,a.[DAY]
      ,a.[TimeInterval]
      ,[pickup_Geohash]
      ,[Num_Jrnys]
      ,[total_passenger_count]
      ,[total_fare]
	  ,[Num_Jrnys_Prev_Day]
      ,[Num_Jrnys_Prev_Week]
      ,[Num_Jrnys_Prev_Hour]
      ,[Num_Jrnys_Prev_HalfHour]
	  ,[Class]
      ,[Temp]
      ,[Heat_Index]
      ,[Windchill]
      ,[Dew_Point]
      ,[Humidity]
      ,[Pressure]
      ,[Visibility]
      ,[Wind_Dir]
      ,[Wind_Speed]
      ,[Gust_Speed]
      ,[Precip]
      ,[Events]
      ,[Conditions]
	  into [DCU Machine Learning].[dbo].[Summarized_inc_weather_Final]
  FROM [DCU Machine Learning].[dbo].[TaxiData_Summarized_With_Lags] a
  left join [DCU Machine Learning].[dbo].[Weather_Final] b
  on a.pickupDate = convert(varchar,b.[pickupDate], 23) and a.TimeInterval = left(convert(varchar, b.[TimeInterval], 108), 5)
	
