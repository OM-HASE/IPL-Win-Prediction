import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestClassifier
import pickle
from sklearn.preprocessing import OneHotEncoder
from sklearn.impute import SimpleImputer

# Load datasets
matches = pd.read_csv('C:/Users/omhas/Documents/IPL Project/IPL_Prediction/data/matches.csv')
deliveries = pd.read_csv('C:/Users/omhas/Documents/IPL Project/IPL_Prediction/data/deliveries.csv')

# Process the data as in the R code
inning_score = deliveries.groupby(['match_id', 'inning']).agg(total_runs=('total_runs', 'sum')).reset_index()
inning_score = inning_score[inning_score['inning'] == 1]
inning_score['target'] = inning_score['total_runs'] + 1

matches = matches.rename(columns={'id': 'id'})
inning_score = inning_score.rename(columns={'match_id': 'id'})
matches = matches.merge(inning_score[['id', 'target']], on='id', how='left')

matches['team1'] = matches['team1'].replace('Delhi Daredevils', 'Delhi Capitals')
matches['team2'] = matches['team2'].replace('Delhi Daredevils', 'Delhi Capitals')
matches['winner'] = matches['winner'].replace('Delhi Daredevils', 'Delhi Capitals')

# Replace team names as done in the R script
team_replacements = {
    'Kings XI Punjab': 'Punjab Kings',
    'Deccan Chargers': 'Sunrisers Hyderabad',
    'Rising Pune Supergiant': 'Pune Warriors',
    'Rising Pune Supergiants': 'Pune Warriors',
    'Gujarat Lions': 'Gujarat Titans'
}

matches['team1'] = matches['team1'].replace(team_replacements)
matches['team2'] = matches['team2'].replace(team_replacements)
matches['winner'] = matches['winner'].replace(team_replacements)

teams2024 = ['Rajasthan Royals', 'Royal Challengers Bangalore', 'Sunrisers Hyderabad', 'Delhi Capitals', 
             'Chennai Super Kings', 'Gujarat Titans', 'Lucknow Super Giants', 'Kolkata Knight Riders',
             'Punjab Kings', 'Mumbai Indians']
matches = matches[matches['team1'].isin(teams2024) & matches['team2'].isin(teams2024) & matches['winner'].isin(teams2024)]

# Further data processing as in the R code
final = pd.merge(matches[['id', 'city', 'team1', 'team2', 'winner', 'target']], deliveries, on='id', how='inner')
final = final[final['inning'] == 2]

# Add features like runs_left, balls_left, etc.
final['current_score'] = final.groupby('id')['total_run'].cumsum()
final['runs_left'] = np.maximum(final['target'] - final['current_score'], 0)
final['balls_left'] = np.maximum(120 - final['over'] * 6 - final['ball'], 0)
final['wickets_left'] = final.groupby('id').cumcount() + 1
final['current_run_rate'] = final['current_score'] * 6 / (120 - final['balls_left'])
final['required_run_rate'] = np.where(final['balls_left'] > 0, final['runs_left'] * 6 / final['balls_left'], 0)

# Result column
final['result'] = (final['batting_team'] == final['winner']).astype(int)

# Prepare the data for the model
features = ['BattingTeam', 'BowlingTeam', 'city', 'runs_left', 'balls_left', 'wickets_left', 
            'current_run_rate', 'required_run_rate', 'target']
X = final[features]
y = final['result']

# One-hot encoding and train-test split
encoder = OneHotEncoder(sparse=False)
X_encoded = encoder.fit_transform(X[['BattingTeam', 'BowlingTeam', 'city']])
X_numeric = X.drop(['BattingTeam', 'BowlingTeam', 'city'], axis=1).values

X_data = np.hstack([X_encoded, X_numeric])

# Imputation
imputer = SimpleImputer(strategy='median')
X_data_imputed = imputer.fit_transform(X_data)

X_train, X_test, y_train, y_test = train_test_split(X_data_imputed, y, test_size=0.3, random_state=42)

# Train the RandomForest model
rf_model = RandomForestClassifier(n_estimators=200, max_features='sqrt')
rf_model.fit(X_train, y_train)

# Save the model and encoders
with open('rf_model.pkl', 'wb') as model_file:
    pickle.dump(rf_model, model_file)
with open('encoder.pkl', 'wb') as encoder_file:
    pickle.dump(encoder, encoder_file)
with open('imputer.pkl', 'wb') as imputer_file:
    pickle.dump(imputer, imputer_file)