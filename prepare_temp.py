import numpy as np
import pandas as pd
import xarray as xr

# Load the dataset
file_path = 'tas_hadukgrid_uk_5km_mon_202301-202312.nc'
ds = xr.open_dataset(file_path)

# Select the temperature data
temperature = ds['tas']

# Flatten latitude, longitude, and time data
latitude = ds['latitude']
longitude = ds['longitude']
lat_flat = np.tile(latitude, (temperature.shape[0], 1))  # Repeat latitudes for each time slice
lon_flat = np.tile(longitude, (temperature.shape[0], 1))  # Repeat longitudes for each time slice

# Flatten the temperature data across time, latitude, and longitude
temperature_flat = temperature.values.flatten()

# Time data
time_flat = np.tile(ds['time'].values, (latitude.size, 1)).flatten()

# Create the DataFrame
df = pd.DataFrame({
    'time': time_flat,
    'latitude': lat_flat.flatten(),
    'longitude': lon_flat.flatten(),
    'temperature': temperature_flat
})

# Filter out NaN values from the DataFrame
df_clean = df.dropna(subset=['temperature'])

# Save the cleaned DataFrame to a CSV file
df_clean.to_csv('temperature_cleaned.csv', index=False)

# Show the first few rows of the cleaned dataframe
print(df_clean.head())

# Fill NaN values with 0
df_filled = df.fillna({'temperature': 0})

# Save the DataFrame with filled values to a CSV file
df_filled.to_csv('temperature_filled.csv', index=False)

# Show the first few rows of the dataframe with filled NaN values
print(df_filled.head())
