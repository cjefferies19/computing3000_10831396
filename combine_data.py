import pandas as pd

# Load the filled CSV files in chunks
rainfall_df = pd.read_csv('rainfall_filled.csv', chunksize=100000)
wind_df = pd.read_csv('wind_filled.csv', chunksize=100000)
temperature_df = pd.read_csv('temperature_filled.csv', chunksize=100000)

# Initialize an empty DataFrame to store the combined data
combined_df = pd.DataFrame()

# Iterate through the chunks and merge them in parts
for chunk_rain, chunk_wind, chunk_temp in zip(rainfall_df, wind_df, temperature_df):
    # Merge the chunks on 'time', 'latitude', and 'longitude'
    temp_combined = pd.merge(chunk_rain, chunk_wind, on=['time', 'latitude', 'longitude'], how='outer')
    temp_combined = pd.merge(temp_combined, chunk_temp, on=['time', 'latitude', 'longitude'], how='outer')

    # Append the merged chunk to the final combined DataFrame
    combined_df = pd.concat([combined_df, temp_combined], ignore_index=True)

# Check the final combined DataFrame
print(f"Combined DataFrame shape: {combined_df.shape}")
print(combined_df.head())

# Save the combined DataFrame to a new CSV file
combined_df.to_csv('combined_weather_data.csv', index=False)

# Show the first few rows of the combined DataFrame
print(combined_df.head())
