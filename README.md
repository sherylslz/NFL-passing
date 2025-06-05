# NFL-passing

This dataset contains pass attempts from weeks 1–9 of the 2022 NFL regular season, courtesy of the nflreadr package (part of the nflverse) and the 2025 NFL Big Data Bowl.

Each row in the dataset corresponds to a pass attempt and the columns are:

game_id: unique identifier for a game

play_id: unique identifier for a play (within a game)

passer_player_name: name of the player that attempted the pass

passer_player_id: unique identifier for the player that attempted the pass

posteam: abbreviation for the team with possession

complete_pass: indicator denoting whether or not the pass was completed

interception: indicator denoting whether or not the pass was intercepted by the defense

yards_gained: yards gained (or lost) by the possessing team, excluding yards gained via fumble recoveries and laterals

touchdown: indicator denoting if the play resulted in a touchdown

pass_location: categorical location of pass

pass_length: categorical length of pass

air_yards: distance in yards perpendicular to the line of scrimmage at where the targeted receiver either caught or didn’t catch the ball

yards_after_catch: distance in yards perpendicular to the yard line where the receiver made the reception to where the play ended

shotgun: indicator for whether or not the play was in shotgun formation

no_huddle: indicator for whether or not the play was in no_huddle formation

defteam: abbreviation for the team on defense

posteam_type: indicating whether the posteam team is home or away

yardline_100: distance in the number of yards from the opponent’s endzone for the posteam

side_of_field: abbreviation for which team’s side of the field the team with possession is currently on
down: down for the given play

qtr: quarter of the game (5 is overtime)

half_seconds_remaining: seconds remaining in the half

game_half: indicating which half the play is in

home_team: abbreviation for the home team

away_team: abbreviation for the away team

home_score: total points scored by the home team

away_score: total points scored by the away team

target_player_id: unique identifier for the intended receiver

target_player_name: name of the intended receiver

had_pass_reception: whether the pass was caught by the intended receiver

motion_since_lineset: whether the intended receiver went in motion after they were initially set at the line of scrimmage

route_ran: route ran by the intended receiver

offense_formation: formation used by the offense

target_x: x coordinate (along the end zone direction) of targeted receiver when the pass arrived (Note: all locations have been standardized so that the possession team is going from left to right. Higher x values means closer to the target end zone)

target_y: y coordinate (along the sideline direction) of targeted receiver when the pass arrived

time_to_throw: time (in seconds) elapsed between snap and pass


Starter Code:

library(tidyverse)
nfl_passes <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/nfl_passes.csv")
