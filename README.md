# Master-s-Thesis
Analysis of the influence of traffic flow and power generation on air pollution.

Choose XGBoost as the model.
(Use stepwise to choose parameters)

And then use LSTM model to predict future parameters.
(LSTM built using the Keras Python package to predict time series steps and sequences.)

Treat the parameters as the input of the XGBoost model.

Finally, do sensitivity analysis.

Example of traffic sensitivity analysis table：

![image](https://github.com/jeff611196/Master-s-Thesis/blob/master/taipei_traffic_pheatmap.png)

The dark area is the area where the 110 groups of minimum PM2.5 conditions meet the more conditions.
