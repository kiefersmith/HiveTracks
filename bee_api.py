import requests
import pandas as pd
from datetime import date

t = date.today()
t_formatted = t.strftime("%Y%m%d%0000000")
month = datetime.timedelta(days=30)
back = t - month
b_formatted = back.strftime("%Y%m%d%0000000")


url = 'http://uoweb1.ncl.ac.uk/api/v1/sensors/data/raw.csv?start_time=20170101000000&end_time=' + t_formatted + '&sensor_type=Bee+Hive&api_key=zvsa1dg8ty8eeknb8ndnz8e0jvsk8yhx8nfz8d8lskfwohzzowzmw0rnv2sr65h64og3n6qoxlrjyvegh9rsnkskgw'
res = requests.get(url)
content = io.StringIO(res.content.decode('utf-8'))
data = pd.read_csv(content)
data['Timestamp'] = pd.to_datetime(data['Timestamp'])
#data = data.pivot(columns = 'Variable', index = data.index)['Value']

data.to_csv("bees.csv")

