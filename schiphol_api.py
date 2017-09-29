'''
Calls Schiphol API to look for current flights
Requires two arguments, both can be found on your profile page:
- app_id (application identifier)
- app_key (application key)

Example usage
python schiphol_api.py --app_id 123abc --app_key 1234567890abcdefghij

Example query URL:
https://api.schiphol.nl/public-flights/flights?app_id=123abc&app_key=1234567890abcdefghij

Full documentation can be found here:
https://developer.schiphol.nl/apis/flight-api/overview

Keys file should be JSON of the format:
{
	"app_id": "app id here",
	"app_key": "app key here"
}
'''

import requests
import sys
import optparse
import time
import json
import pandas as pd

keys = json.load(open('/path/to/keys/schiphol_keys.json', 'r'))

def callFlights(options):
	url = 'https://api.schiphol.nl/public-flights/flights'
	querystring = dict(app_id=keys['app_id'], app_key=keys['app_key'])
	headers = dict(resourceversion='v3')
	
	responses = list()
	page = 0
	good_response = True
	
	while good_response:
		if page % 5 == 0:
			print('\nWaiting a few seconds...\n')
			time.sleep(5)
		querystring['page'] = page
		response = requests.request('GET', url, headers=headers, params=querystring)
		print('Trying page {}: response code: {}'.format(page, response.status_code))
		if response.status_code == 200:
			page += 1
			responses.extend(response.json()['flights'])
		if response.status_code != 200 or page > 2:
			df = pd.DataFrame(responses)
			df.drop('schemaVersion', axis=1, inplace=True)
			df.to_csv('druktemeter_ftp/raw_data/schiphol_api/flights.csv', sep=';', index=False)
			break


# nog dictionaries verwijderen, anders werkt drop_duplicates() niet
def callDestinations(options):
	url = 'https://api.schiphol.nl/public-flights/destinations'
	querystring = dict(app_id=keys['app_id'], app_key=keys['app_key'])
	headers = dict(resourceversion='v1')
	responses = list()
	page = 0
	good_response = True
	while good_response:
		if page % 5 == 0:
			print('\nWaiting a few seconds...\n')
			time.sleep(5)
		querystring['page'] = page
		response = requests.request('GET', url, headers=headers, params=querystring)
		print('Trying page {}: response code: {}'.format(page, response.status_code))
		if response.status_code == 200:
			page += 1
			responses.extend(response.json()['destinations'])
		if response.status_code != 200:# or page > 2:
			df = pd.DataFrame(responses)
			df['publicName_english'] = [dest['english'] for dest in df.publicName]
			df['publicName_dutch'] = [dest['dutch'] for dest in df.publicName]
			df.drop('schemaVersion', axis=1, inplace=True)
			df.drop('publicName', axis=1, inplace=True)
			# df = df.drop_duplicates()
			df.to_csv('druktemeter_ftp/raw_data/schiphol_api/destinations.csv', sep=';', index=False)
			break


def callAircrafTypes(options):
	url = 'https://api.schiphol.nl/public-flights/aircrafttypes'
	querystring = dict(app_id=keys['app_id'], app_key=keys['app_key'])
	headers = dict(resourceversion='v1')
	responses = list()
	page = 0
	good_response = True
	
	while good_response:
		if page % 5 == 0:
			print('\nWaiting a few seconds...\n')
			time.sleep(5)
		querystring['page'] = page
		response = requests.request('GET', url, headers=headers, params=querystring)
		print('Trying page {}: response code: {}'.format(page, response.status_code))
		if response.status_code == 200:
			page += 1
			responses.extend(response.json()['aircraftTypes'])
		if response.status_code != 200:# or page > 2:
			df = pd.DataFrame(responses)
			df.drop('schemaVersion', axis=1, inplace=True)
			# df = df.drop_duplicates()
			df.to_csv('druktemeter_ftp/raw_data/schiphol_api/aircrafttypes.csv', sep=';', index=False)
			break


if __name__ == '__main__':
	parser = optparse.OptionParser()
	parser.add_option('-o', '--app_use', dest='app_use', help='API method')
	(options,args) = parser.parse_args()
	
	if options.app_use == 'scraper':
		callFlights(options)
	if options.app_use == 'destinations':
		callDestinations(options)
	if options.app_use == 'aircrafttypes':
		callAircrafTypes(options)
	
	if options.app_use is None:
		callFlights(options)
		callDestinations(options)
		callAircrafTypes(options)