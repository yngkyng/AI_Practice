from flask import Flask,render_template,request
from keras.models import load_model
import numpy as np
import joblib


app = Flask(__name__)


@app.route('/', methods=['GET'])
def main():
    return render_template('rides/input.html')


@app.route('/result', methods=['POST'])
def result():
    model = load_model('c:/vscode/data/rides/rides.h5')
    scaler = joblib.load('c:/vscode/data/rides/scaler.sav')
    week = request.form['week']
    if week == "1":
        weekend = '주말'
    else:
        weekend = '평일'
    child = int(request.form['child'])
    distance = int(request.form['distance'])
    rides = int(request.form['rides'])
    games = int(request.form['games'])
    wait = int(request.form['wait'])
    clean = int(request.form['clean'])
    test_set = np.array([week, child, distance, rides,
                         games, wait, clean]).reshape(1, 7)
    test_set_scaled = scaler.transform(test_set)
    rate = model.predict(test_set_scaled)
    if rate >= 0.5:
        result = '만족'
    else:
        result = '불만족'
    return render_template('rides/result.html',
                           rate='{:.2f}%'.format(rate[0][0] * 100), result=result,
                           weekend=weekend, child=child,
                           distance=distance, rides=rides, games=games, wait=wait,
                           clean=clean)


if __name__ == '__main__':
    app.run(port=8000, threaded=False)