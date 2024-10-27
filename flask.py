from flask import Flask, render_template

app = Flask(__name__)

@app.route('/match-prediction')
def match_prediction():
    team1 = "Team A"
    team2 = "Team B"
    probability1 = 65
    probability2 = 35
    return render_template('your_template.html', team1=team1, team2=team2, probability1=probability1, probability2=probability2)

if __name__ == '__main__':
    app.run(debug=True)
