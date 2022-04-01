#Schema: Colony_behavior
#Tables: Colony_fa, Colony_maze
#Code aggregates and joins colony_fa and colony_maze | allows analysis of behavioral patterns in both fa and maze

#List databases
SHOW databases;

#Choose database
USE colony_behavior;

#View all in colony_fa & colony_maze
SELECT *
FROM colony_fa;

SELECT *
FROM colony_maze;

#Aggregating colony_maze table so that maze_count is sum of counts in each section at each timepoint (zt)
SELECT 
colony
, day
, zt
,SUM(maze_count) AS maze_agg
FROM colony_maze
GROUP BY colony, day, zt;

#Count # of observations in colony_fa and aggregated colony_maze
SELECT COUNT(colony)
FROM colony_fa; #288 observations

SELECT COUNT(colony)
FROM colony_maze
GROUP BY colony AND day AND zt; #2356 observations

#Join colony_fa & colony_maze | inner join because there are non-matching missing values in each table
SELECT
f.colony
, f.day
, f.zt
, f.fa_count
, m.maze_count
FROM colony_fa f
INNER JOIN colony_maze m ON f.colony = m.colony 
AND f.day = m.day
AND f.zt = m.zt;

#Add colony fa_count to maze_count as total_count
SELECT
f.colony
, f.day
, f.zt
, f.fa_count
, m.maze_count
, (f.fa_count + m.maze_count) AS total_count
FROM colony_fa f
INNER JOIN colony_maze m ON f.colony = m.colony 
AND f.day = m.day
AND f.zt = m.zt;

