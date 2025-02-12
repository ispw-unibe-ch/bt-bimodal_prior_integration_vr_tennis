import csv
import pandas as pd
import numpy as np
import os

# clean raw data for day 1, 2, and 3
for day_number in range(1, 4):
    # Specify the path to the raw data files
    #there were two different files for each day, participants 0-15 (1/2) and 16-31 (2/2)
    # path of the current file
    file_path = __file__
    current_folder = os.path.dirname(file_path)
    parent_folder = os.path.abspath(os.path.join(current_folder, '..'))
    day_1_2 = f'raw_data\\vr_tennis_fs24_day{day_number}_1_2\\combined_events.csv'
    day_2_2 = f'raw_data\\vr_tennis_fs24_day{day_number}_2_2\\combined_events.csv'
    day_1_2_path = os.path.join(parent_folder,day_1_2)
    day_2_2_path = os.path.join(parent_folder,day_2_2)

    # Read the CSV file and save it as a data frame
    df1 = pd.read_csv(day_1_2_path)
    df2 = pd.read_csv(day_2_2_path)

    # Drop the rows that you don't need, it is always 3 times the same data in the raw data
    df1 = df1.drop_duplicates(subset='trial', keep='last')
    df2 = df2.drop_duplicates(subset='trial', keep='last')

    # Extract the trial number from the "trial" column
    df1['trial_number'] = df1['trial'].str.extract(r'_(\d+)_S')
    df1['trial_number'] = df1['trial_number'].astype(int)
    df2['trial_number'] = df2['trial'].str.extract(r'_(\d+)_S')
    df2['trial_number'] = df2['trial_number'].astype(int)

    # Extract the subject number from the "trial" column
    df1['subject'] = df1['trial'].str.extract(r'(\d+)_')
    df2['subject'] = df2['trial'].str.extract(r'(\d+)_')

    #subject 3 was wrongly labeled as 19 in the experiment (only on day 1)
    if day_number == 1:
        df1.loc[df1['subject'] == "19", "subject"] = "3"

    # Concatenate df1 and df2
    df = pd.concat([df1, df2]) 

    # Read in the excel protocol file of the experiment
    protocol_file = f'experimental_protocols\\vr_tennis_exp_fs24_protocol_day{day_number}.xlsx'
    protocol_file_path = os.path.join(parent_folder,protocol_file)
    protocol_df = pd.read_excel(protocol_file_path, sheet_name='experiment')

    # initialize the column "ball_position" and "condition"
    df['ball_position'] = 0.0
    df['condition'] = "not_defined"

    trial_number_column = df.columns.get_loc("trial_number")
    ball_position_number_column = df.columns.get_loc("ball_position")
    condition_number_column = df.columns.get_loc("condition")

    # Fill the "ball_position" and "condition" columns with the values from the protocol file
    for i in range(len(df)):
        trial_number = df.iloc[i,trial_number_column]
        #check if it is empty
        if not protocol_df.loc[protocol_df['trial_number'] == trial_number, 'ball_positions'].empty:
            #read out ball_position
            ball_position = protocol_df.loc[protocol_df['trial_number'] == trial_number, 'ball_positions'].values
            df.iloc[i, ball_position_number_column] = float(ball_position[0])
            #read out condition
            condition = protocol_df.loc[protocol_df['trial_number'] == trial_number, 'condition'].values
            df.iloc[i, condition_number_column] = condition[0]
        else:
            df.iloc[i, ball_position_number_column] = np.nan
            df.iloc[i, condition_number_column] = np.nan


    # Select the desired columns from df for the output file
    selected_columns = df[['subject', 'horizontal_difference', 'ball_position', 'trial_number', 'condition']]
    selected_columns['subject'] = selected_columns['subject'].astype(int)

    #subject 19 was consequently labeled as 20 in the experiment (only on day 1)
    if day_number == 1:
        selected_columns.loc[selected_columns['subject'] == 20, "subject"] = 19

    #delete all rows  with other vp than on the following list (all other vp were empty or did not finish the experiment)
    vp_list = list(range(1, 13)) + list(range(15, 20)) + list(range(22, 29))
    selected_columns = selected_columns[selected_columns['subject'].isin(vp_list)].copy(deep=True).reset_index(drop=True)

    ## Check if the hit was during the backswing
    # Specify the path to the backswing_check data file
    backswing_check = f'raw_data\\overall_back_swing_check\\overall_back_swing_check.csv'
    backswing_check_path = os.path.join(parent_folder,backswing_check)
    backswing_check_file = pd.read_csv(backswing_check_path)
    if day_number == 1:
        backsing_check_session = backswing_check_file[backswing_check_file['session'] == "_Session1_d01_"]
    elif day_number == 2:
        backsing_check_session = backswing_check_file[backswing_check_file['session'] == "_Session2_d02_"]
    elif day_number == 3:
        backsing_check_session = backswing_check_file[backswing_check_file['session'] == "_Session3_d03_"]
    else:
        print("Error: day number not in range 1-3")

    # Add a new column "backswing" to output_control
    selected_columns['backswing'] = np.False_

    # Iterate through each row and update the "backswing" column
    for index, row in selected_columns.iterrows():
        subject = "vp" + str(row['subject'])
        trial_number = row['trial_number']
        backswing_entry = backsing_check_session[(backsing_check_session['vp'] == subject) & (backsing_check_session['trial_number'] == trial_number)]['backswing'].values
        if len(backswing_entry) > 0:
            backswing_entry = backswing_entry[0]
            selected_columns.at[index, 'backswing'] = backswing_entry
    # Export the selected columns to a CSV file
    export_path = os.path.join(parent_folder, f'data\\output_day{day_number}.csv')
    selected_columns.to_csv(export_path, index=False)
