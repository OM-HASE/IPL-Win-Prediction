from flask import Flask, render_template, request
import pickle
import numpy as np

app = Flask(__name__)

# Load trained model
with open('model.pkl', 'rb') as file:
    model = pickle.load(file)

@app.route('/')
def index():
    return render_template('index.html')

@app.route('/predict', methods=['POST'])
def predict():
    batting_team = request.form['batting_team']
    bowling_team = request.form['bowling_team']
    city = request.form['city']
    runs_left = int(request.form['runs_left'])
    balls_left = int(request.form['balls_left'])
    wickets_left = int(request.form['wickets_left'])
    current_run_rate = float(request.form['current_run_rate'])
    required_run_rate = float(request.form['required_run_rate'])
    target = int(request.form['target'])

    # Create input array
    input_data = np.array([[batting_team, bowling_team, city, runs_left, balls_left, wickets_left, current_run_rate, required_run_rate, target]])

    # Predict the result
    prediction = model.predict_proba(input_data)

    probability1 = prediction[0][0] * 100
    probability2 = prediction[0][1] * 100

    return render_template('result.html', team1=batting_team, probability1=probability1, team2=bowling_team, probability2=probability2)

if __name__ == '__main__':
    app.run(debug=True)
