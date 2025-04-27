import numpy as np
import pandas as pd
import xarray as xr

# Load the dataset
file_path = 'rainfall_hadukgrid_uk_5km_mon_202301-202312.nc'
ds = xr.open_dataset(file_path)

# Select the rainfall data
rainfall = ds['rainfall']

# Flatten latitude, longitude, and time data
latitude = ds['latitude']
longitude = ds['longitude']
lat_flat = np.tile(latitude, (rainfall.shape[0], 1)) # Repeat latitudes for each time slice
lon_flat = np.tile(longitude, (rainfall.shape[0], 1)) # Repeat longitudes for each time slice

# Flatten the rainfall data across time, latitude, and longitude
rainfall_flat = rainfall.values.flatten()

# Time data
time_flat = np.tile(ds['time'].values, (latitude.size, 1)).flatten()

# Create the dataframe
df = pd.DataFrame({
    'time': time_flat,
    'latitude': lat_flat.flatten(),
    'longitude': lon_flat.flatten(),
    'rainfall': rainfall_flat
})

# Filter out NaN values from the dataframe
df_clean = df.dropna(subset=['rainfall'])

# Save the cleaned data to a CSV file
df_clean.to_csv('rainfall_cleaned.csv', index=False)

# Fill NaN values with 0
df_filled = df.fillna({'rainfall': 0})

# Save the dataframe with filled values to a CSV file
df_filled.to_csv('rainfall_filled.csv', index=False)
