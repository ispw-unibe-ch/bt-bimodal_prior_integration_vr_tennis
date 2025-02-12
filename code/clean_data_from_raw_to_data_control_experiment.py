import csv
import pandas as pd
import numpy as np
import os

# Specify the path to the raw data files
#there were two different files for each day, participants 0-15 (1/2) and 16-31 (2/2)
# path of the current file
file_path = __file__
current_folder = os.path.dirname(file_path)
parent_folder = os.path.abspath(os.path.join(current_folder, '..'))
control_1_2 = f'raw_data\\vr_tennis_hs24_control_exp_1_2\\combined_events.csv'
control_2_2 = f'raw_data\\vr_tennis_hs24_control_exp_2_2\\combined_events.csv'
control_1_2_path = os.path.join(parent_folder, control_1_2)
control_2_2_path = os.path.join(parent_folder, control_2_2)

# Read the CSV file into a DataFrame
df1 = pd.read_csv(control_1_2_path)
df2 = pd.read_csv(control_2_2_path)

# Drop the rows that you don't need, it is always 3 times the same row
df1 = df1.drop_duplicates(subset='trial', keep='last')
df2 = df2.drop_duplicates(subset='trial', keep='last')

# Concatenate df1 and df2
df = pd.concat([df1, df2]) 

# Extract the trial number from the "trial" column
df['trial_number'] = df['trial'].str.extract(r'_(\d+)_S')
df['trial_number'] = df['trial_number'].astype(int)

# Extract the subject number from the "trial" column
df['subject'] = df['trial'].str.extract(r'(\d+)_')

# Read in the excel protocol file of the experiment
protocol_file = f'experimental_protocols\\vr_tennis_exp_hs24_protocol_control.xlsx'
protocol_file_path = os.path.join(parent_folder,protocol_file)
protocol_df = pd.read_excel(protocol_file_path, sheet_name='experiment')

# initialize the column "ball_position"
df['ball_position'] = 0.0
df['condition'] = "not_defined"

trial_number_column = df.columns.get_loc("trial_number")
ball_position_number_column = df.columns.get_loc("ball_position")
condition_number_column = df.columns.get_loc("condition")

for i in range(len(df)):
    trial_number = df.iloc[i,trial_number_column]
    if not protocol_df.loc[protocol_df['trial_number'] == trial_number, 'ball_positions'].empty:
        ball_position = protocol_df.loc[protocol_df['trial_number'] == trial_number, 'ball_positions'].values
        df.iloc[i, ball_position_number_column] = float(ball_position[0])
        condition = protocol_df.loc[protocol_df['trial_number'] == trial_number, 'condition'].values
        df.iloc[i, condition_number_column] = condition[0]
    else:
        df.iloc[i, ball_position_number_column] = np.nan
        df.iloc[i, condition_number_column] = np.nan

# Select the desired columns from df
selected_columns = df[['subject', 'horizontal_difference', 'ball_position', 'trial_number', 'condition']]
selected_columns['subject'] = selected_columns['subject'].astype(int)

#delete all rows  with other vp than on the following list
vp_list = list(range(1, 15)) + list(range(16, 21)) + list(range(22, 29))
selected_columns = selected_columns[selected_columns['subject'].isin(vp_list)]

## Check if the hit was during the backswing
# Specify the path to the backswing_check data file
backswing_check = f'raw_data\\overall_back_swing_check\\overall_back_swing_check_control_experiment.csv'
backswing_check_path = os.path.join(parent_folder,backswing_check)
backswing_check_file = pd.read_csv(backswing_check_path)
backsing_check_session = backswing_check_file

# Add a new column "backswing" to output_control
selected_columns['backswing'] = None

# Iterate through each row and update the "backswing" column
for index, row in selected_columns.iterrows():
    subject = "vp" + str(row['subject'])
    trial_number = row['trial_number']
    backswing_entry = backsing_check_session[(backsing_check_session['vp'] == subject) & (backsing_check_session['trial_number'] == trial_number)]['backswing'].values
    if len(backswing_entry) > 0:
        backswing_entry = backswing_entry[0]
        selected_columns.at[index, 'backswing'] = backswing_entry

# Export the selected columns to a CSV file
export_path = os.path.join(parent_folder, f'data\\output_control.csv')
selected_columns.to_csv(export_path, index=False)
