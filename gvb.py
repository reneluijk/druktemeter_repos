def start_end(t):
	# function to convert string of timespan into hours
	
	# special cases
	if t == '22-ED':
		start = 22
		end = 2
	if t == 'overige tijdstippen':
		start = np.nan
		end = np.nan
	if 'uur' in t:
		t = re.sub(' uur', '', t)
		t = re.split('-', t)
		start = int(t[0])
		end = int(t[1])
	elif 'he' in t:
		t = re.sub('he', '', t)
		if ',' in t:
			t = re.sub(',5', '', t)
			start = float(t) + 0.5
			end = float(t) + 1.5
		else:	
			start = float(t)
			end = float(t) + 1.0
	elif 'ha' in t:
		t = re.sub('ha', '', t)
		if ',' in t:
			t = re.sub(',5', '', t)
			start = float(t) + 0.5
			end = float(t) + 1.0
		else:
			start = float(t)
			end = float(t) + 0.5
	
	return start, end


def clean_ritten():
	# function to clean up some things in ritten data frame
	
	global ritten
	# remove overige tijdstippen
	ritten = ritten.loc[ritten.tijdstip != 'overige tijdstippen', :]
	# drop unnecessary columns
	ritten.drop('haltenaam', axis=1, inplace=True)
	# return result
	return ritten


def sum_haltenaam():
	# Sum number of passengers by halte
	
	global ritten, locaties
	
	# add haltenamen instaphalte
	ritten = ritten.merge(locaties[['OrtNr', 'haltenaam']], on='OrtNr')
	ritten = ritten.rename(columns={'haltenaam': 'instaphalte', 'OrtNr': 'instaphalte_ort'})
	
	# add haltenamen uitstaphalte
	ritten = ritten.merge(locaties[['OrtNr', 'haltenaam']], left_on='OrtNr_uit', right_on='OrtNr')
	ritten = ritten.rename(columns={'haltenaam': 'uitstaphalte', 'OrtNr_uit': 'uitstaphalte_ort'})
	
	# sum per combination
	tot_ritten = ritten.groupby(['Weekdag', 'tijdstip', 'instaphalte', 'uitstaphalte'])['tot_ritten'].sum().reset_index()
	
	# return result
	return tot_ritten


def add_start_end_times():
	# Add start and end times per entry
	# Times indicate time period in which the number
	# of passengers are measured
	
	global ritten, locaties
	
	# create start and end times
	times = [start_end(t) for t in ritten.tijdstip]
	ritten['start_time'] = [t[0] for t in times]
	ritten['end_time'] = [t[1] for t in times]
	
	# drop tijdstip
	ritten.drop('tijdstip', axis=1, inplace=True)
	
	# return results
	return ritten


def average_june():
	# Some days have occured more often in this period
	# Average the number of passengers over the time period of the data
	
	global ritten
	weekdagen = pd.read_excel('druktemeter/druktemeter_ftp/raw_data/GVB/Werkdag_aantal.xlsx')
	weekdagen['Weekdag'] = [x.lower() for x in weekdagen.Weekdag]
	ritten = ritten.merge(weekdagen, on='Weekdag')
	ritten['mean_ritten'] = ritten.tot_ritten / ritten.aantal_dagen
	ritten.drop('tot_ritten', axis=1, inplace=True)
	ritten.drop('aantal_dagen', axis=1, inplace=True)
	return ritten


def get_half_hours(row):
	# Get vector of the half hours per time period
	
	st = row['start_time']
	et = row['end_time']
	if st == 22 and et == 2:
		return [22.5, 23, 23.5, 24, 0.5, 1, 1.5, 2]
	else:
		return np.arange(st + 0.5, et + 0.5, 0.5)


def half_hour_stats():
	# Get number of passengers per half hour
	
	global ritten
	ritten['half_hours'] = [get_half_hours(row) for index, row in ritten.iterrows()]
	ritten['ritten_per_half_hour'] = np.array(ritten.mean_ritten) / np.array([len(hh) for hh in ritten.half_hours])
	ritten.drop('start_time', axis=1, inplace=True)
	ritten.drop('end_time', axis=1, inplace=True)
	return ritten


def add_lat_lon():
	# Add geographical coordinates
	
	global ritten_long, mean_locaties
	
	# add info for start location
	mean_locaties = mean_locaties.rename(columns={'haltenaam': 'instaphalte', 'lat': 'instaphalte_lat', 'lng': 'instaphalte_lng'}, inplace=True)
	ritten_long = ritten_long.merge(mean_locaties, on='instaphalte')
	
	# add info for end location
	mean_locaties = mean_locaties.rename(columns={'instaphalte': 'uitstaphalte', 'instaphalte_lat': 'uitstaphalte_lat', 'instaphalte_lng': 'uitstaphalte_lng'})
	ritten_long = ritten_long.merge(mean_locaties, on='uitstaphalte')
	
	# return result
	return ritten_long


def hour_stats(row):
	# Number of passengers per hour
	
	global cols
	tmp = pd.DataFrame([row]*len(row['half_hours']), columns=cols)
	tmp['half_hours'] = tmp.half_hours.iloc[0]
	tmp['hour'] = [np.ceil(x) for x in tmp.half_hours]
	tmp = tmp.groupby(['Weekdag', 'instaphalte', 'uitstaphalte', 'hour'])['ritten_per_half_hour'].sum().reset_index()
	return tmp


import re
import pandas as pd
import numpy as np


# if __name__ == '__main__':

# read locations
locaties = pd.read_excel('druktemeter/druktemeter_ftp/raw_data/GVB/Ortnr - coordinaten (ingangsdatum dec 2015) met LAT LONG.xlsx')

# mean locaties
locaties.drop('X_COORDINAAT', axis=1, inplace=True)
locaties.drop('Y_COORDINAAT', axis=1, inplace=True)
locaties = locaties.rename(columns={'LAT': 'lat', 'LONG': 'lng'})

if not os.path.isfile('druktemeter/druktemeter_ftp/parsed_data/GVB/mean_locations.csv'):
	mean_locaties = locaties.groupby('haltenaam')[['lat', 'lng']].mean().reset_index()
	# write mean locations
	mean_locaties.to_csv('druktemeter/druktemeter_ftp/parsed_data/GVB/mean_locations.csv', sep=';', index=False)
else:
	mean_locaties = pd.read_csv('druktemeter/druktemeter_ftp/parsed_data/GVB/mean_locations.csv', sep=';')

if not os.path.isfile('druktemeter/druktemeter_ftp/parsed_data/GVB/mean_locations.csv'):
	ritten = pd.read_excel('druktemeter/druktemeter_ftp/raw_data/GVB/GVB ritten juni 2016.xlsx')
	
	# clean ritten
	ritten = clean_ritten()
	
	# sum per haltenaam, not ort number
	ritten = sum_haltenaam()
	
	# add start and end times per time
	ritten = add_start_end_times()
	
	# take average of ritten, divide by number of days in that month
	ritten = average_june()
	
	# half hour stats
	ritten = half_hour_stats()
	
	# creat long format
	cols = ritten.columns
	ritten_long = [hour_stats(row) for index, row in ritten.iterrows()]
	ritten_long = pd.concat(ritten_long)
	ritten_long = ritten_long.rename(columns={'ritten_per_half_hour': 'tot_ritten'})
	
	# temporary save long format
	ritten_long.to_csv('druktemeter/druktemeter_ftp/parsed_data/GVB/ritten_long.csv', sep=';', index=False)
	
	# add lat lon information
	ritten_long = add_lat_lon()
	
	# write to csv (semi-colon separated)
	ritten_long.to_csv('druktemeter/druktemeter_ftp/parsed_data/GVB/ritten_long.csv', sep=';', index=False)
else:
	ritten_long = pd.read_csv('druktemeter/druktemeter_ftp/parsed_data/GVB/ritten_long.csv', sep=';')