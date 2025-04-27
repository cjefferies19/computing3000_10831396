import numpy as np
import pandas as pd
import xarray as xr

# Load the dataset
file_path = 'sfcWind_hadukgrid_uk_5km_mon_202301-202312.nc'
ds = xr.open_dataset(file_path)

# Select the wind data
wind = ds['sfcWind']

# Flatten latitude, longitude, and time data
latitude = ds['latitude']
longitude = ds['longitude']
lat_flat = np.tile(latitude, (wind.shape[0], 1))  # Repeat latitudes for each time slice
lon_flat = np.tile(longitude, (wind.shape[0], 1))  # Repeat longitudes for each time slice

# Flatten the wind data across time, latitude, and longitude
wind_flat = wind.values.flatten()

# Time data
time_flat = np.tile(ds['time'].values, (latitude.size, 1)).flatten()

# Create the dataframe
df = pd.DataFrame({
    'time': time_flat,
    'latitude': lat_flat.flatten(),
    'longitude': lon_flat.flatten(),
    'wind': wind_flat
})

# Filter out NaN values from the dataframe
df_clean = df.dropna(subset=['wind'])

# Save the cleaned dataframe to a CSV file
df_clean.to_csv('wind_cleaned.csv', index=False)

# Fill NaN values with 0
df_filled = df.fillna({'wind': 0})

# Save the dataframe with filled values to a CSV file
df_filled.to_csv('wind_filled.csv', index=False)
