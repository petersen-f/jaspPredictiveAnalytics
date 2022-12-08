import QtQuick 2.11
import QtQuick.Layouts 1.3
import JASP.Controls 1.0
import JASP.Widgets 1.0

Form
{

	VariablesForm
	{

		AvailableVariablesList	{ name: "allVariablesList" }
		AssignedVariablesList	{ name: "dependent";		title: qsTr("Dependent Variable");	suggestedColumns: ["scale"];	singleVariable: true}
		AssignedVariablesList	{ name: "time";				title: qsTr("Time");				suggestedColumns: ["nominal"];	singleVariable: true}
		AssignedVariablesList	{ name: "covariates";		title: qsTr("Covariates");			suggestedColumns: ["scale"];	allowedColumns: ["scale"]}
		AssignedVariablesList 	{ name: "factors";		title: qsTr("Factors");		allowedColumns: ["ordinal", "nominal", "nominalText"]}
		AssignedVariablesList	{ name: "trainingIndicator"; title: qsTr("Include in Training"); suggestedColumns: ["scale"]; singleVariable: true}
	}


	Section
	{
		title: qsTr("Time Series Descriptives")

		Group
		{
			title: qsTr("Error Bound Selection Method")
			DropDown
			{
				name: "errorBoundMethodDrop"
				id: methodSelection
				values:
 				[
					{ label: "Data Based", value: "stdDevBound"},
    				{ label: "Manual Bounds", value: "manualBound"}
  				]
			}

			Group
			{
				visible: methodSelection.currentValue == "manualBound"

				RadioButtonGroup
				{
					name: "manualBoundMethod"
					RadioButton
					{
						value: "manualBoundUniform"
						checked: true
						childrenOnSameRow: true
						Group
						{
							columns: 2
							DoubleField{name: "manualBoundMean";label: "Mean"; negativeValues: true}
							DoubleField{name: "manualBoundErrorBound";label: "+/-"}
						}




					}
					RadioButton
					{
						value: "manualBoundCustom"
						childrenOnSameRow: true
						Group
						{
							columns: 1
							DoubleField{name: "manualUpperBound";label: "Upper bound"; negativeValues: true}
							DoubleField{name: "manualLowerBound";label: "Lower bound"; negativeValues: true}
						}

					}
				}

				//IntegerField{name: "controlMean"; label: qsTr("Control Mean"); defaultValue: Null; negativeValues: true}
				//IntegerField{name: "controlError"; label: qsTr("Control Error"); defaultValue: 0; negativeValues: false}
			}

			Group
			{
				visible: methodSelection.currentValue == "stdDevBound"
				DoubleField{name: "sigmaBound"; label: qsTr("σ threshold"); defaultValue: 2}
				CheckBox
				{
					name: "trimmedMeanCheck"
					label: qsTr("Trimmed mean")
					DoubleField{name: "trimmedMeanPercent";label: qsTr("Percent");	max: 0.5}
				}
					CheckBox
					{
						name: "controlPeriodCheck"
						label: qsTr("Custom Period")
						childrenOnSameRow: false
						// fix that end period is from start to nrow of series
						Group
						{
							columns: 2
							IntegerField{name:"controlPeriodStart"; label: qsTr("Start"); defaultValue: 0}
							IntegerField{name:"controlPeriodEnd"; label: qsTr("End"); defaultValue: 0}

						}

					}


			}






		}
		Group
		{
			title: "Control Plots"

			CheckBox
			{
				name: "controlPlotCheck"
				label: qsTr("Display control chart")
				id: controlPlotCheckbox
				checked: true


				CheckBox
				{
					name: "controlSpreadPointsEqually"
					label: qsTr("Spread points equally")
					checked: true
				}
				RadioButtonGroup

            	{
                	name: "controlLineType"
                	radioButtonsOnSameRow: true
                	RadioButton { value: "points";	label: qsTr("Points") }
                	RadioButton { value: "line";	label: qsTr("Line"); checked: true }
                	RadioButton { value: "both";	label: qsTr("Both")}
            	}

				RadioButtonGroup
				{
					name: "xAxisLimit"
					title: "Y-Axis Limit:"
					radioButtonsOnSameRow: true
					RadioButton { value: "allData";	label: qsTr("All data") }
					RadioButton { value: "controlBounds";	label: qsTr("Control bounds") }
				}

				CheckBox{name: "controlPlotGrid"; label: qsTr("Enable grid")}

				CheckBox
				{
					name: "controlPlotZoomCheck"
					label: qsTr("Custom Plot Focus")
					childrenOnSameRow: false
					enabled: controlPlotCheckbox.checked
					// fix that end period is from start to nrow of series
					Group
					{
						columns: 2
						IntegerField{name:"zoomPeriodStart"; label: qsTr("Start"); defaultValue: 0}
						IntegerField{name:"zoomPeriodEnd"; label: qsTr("End"); defaultValue: 0}

					}

				}
			}
		}


	}
	Section
	{
		title: qsTr("Diagnostics")

		columns: 2


		Group
		{
			title: qsTr("Tables")

			CheckBox
			{
				name: "summaryStatsTableCheck"
				label: "Summary statistics"
			}
			CheckBox
			{
				name: "outlierTableCheck"
				label: "Outlier table"
				CheckBox {name: "outlierTableTransposeCheck"; label: "Transpose table"}
				CheckBox
				{
					name: "outlierTableFocusCheck"
					label: qsTr("Custom Table Focus")
					childrenOnSameRow: false
					enabled: "summaryStatsTableCheck".checked
					// fix that end period is from start to nrow of series
					Group
					{
						columns: 2
						IntegerField{name:"outLierTableStart"; label: qsTr("Start"); defaultValue: 0}
						IntegerField{name:"outLierTableEnd"; label: qsTr("End"); defaultValue: 0}

					}

				}
			}

		}

		Group
		{
			title: qsTr("Plots")

				CheckBox
				{
					name: "outlierHistogramCheck"
					label: qsTr("Histogram")
					//CheckBox
					//{
					//	name: "outlierHistogramDensity"
					//	label: qsTr("Show densities")
					//}
				}

			CheckBox
			{
				name: "acfPlotCheck"
				label: "Autocorrelation function"
				IntegerField{name:"acfLagsMax"; label: qsTr("Lags"); defaultValue: 30}
				CheckBox{name: "acfPartialCheck";label: qsTr("Partial autocorrelation function")}
			}

		}
	}


	Section
	{
		title: qsTr("Feature Engineering")

		IntegerField{name: "featEngLags";label: "Nr. of lags";defaultValue: 0; min: 0}

		CheckBox{name: "featEngAutoTimeBased"; label: "Automatic time-based features"}

		//CheckBox{
		//    name: "featEngAggregateTime"
		//    label: "Aggregate data every"
		//    childrenOnSameRow: true
		//    IntegerField{name: "featEngAggWindow"; afterLabel: qsTr("minutes"); defaultValue: 2; min:2}
		//}

		Group
		{
			Layout.columnSpan: 2
			CheckBox{name: "featEngRemoveZV"; label: qsTr("Remove zero-variance variables")}
			CheckBox{
				name: "featEngRemoveCor"
				label: qsTr("Remove variables that are more stronger correlated than:")
				childrenOnSameRow: true
				DoubleField{ name: "featEngRemoveCorAbove"; defaultValue: 0.8}
			}
		}

	}
	Section
    {
        title: qsTr("Forecast Evaluation")
		Group
		{
			Layout.columnSpan: 2
			columns:2
			Group
        {
			title: qsTr("Evaluation Plan")
            //Layout.columnSpan: 1
            IntegerField{name: "resampleForecastHorizon"; id: "resampleForecastHorizon";  label: qsTr("Prediction window");defaultValue: 100}
            IntegerField{name: "resampleInitialTraining"; label: qsTr("Training window"); defaultValue: resampleForecastHorizon.value*2}

			RadioButtonGroup
			{
				name: "resampleSliceStart"
				title: qsTr("Select slices from:")
				radioButtonsOnSameRow: true
				RadioButton{ value: "head"; label: qsTr("Start")}
				RadioButton{ value: "tail"; label: qsTr("End"); checked: true}
			}
            IntegerField{name: "resampleMaxSlice"; id: "maxSlices"; label: qsTr("Maximum training slices");defaultValue:5}
            CheckBox{name: "resampleCumulativeCheck"; label: qsTr("Cumulative training")}

			CheckBox
			{
				name: "resampleCheckShowTrainingPlan"
				label: qsTr("Show evaluation plan")
				CheckBox{ name: "resamplePlanPlotEqualDistance"; label: qsTr("Spread points equally"); checked: true}
				IntegerField{name: "resamplePlanPlotMaxPlots"; label: "Max slices shown:"; defaultValue: 5; max: maxSlices.value ;min:1}
			}
        }
		}





		Group
		{
			title: qsTr("Model Choice")
			Layout.columnSpan: 2

			VariablesForm
			{
				preferredHeight: jaspTheme.smallDefaultVariablesFormHeight

				AvailableVariablesList
				{
					name: "modelSelection"
					width: preferencesModel.uiScale * 300
					source: [{values: [	{label : qsTr("linear regression - y ~ time"), value: "lmSpike"},
										{label : qsTr("linear regression - regression"), value: "lmSpikeReg"},
										{label : qsTr("linear regression - regression + lag"), value: "lmSpikeRegLag"},
										{label : qsTr("bsts - linear trend model"), value: "bstsLinear"},
										{label : qsTr("bsts - linear trend model - regression"), value: "bstsLinearReg"},
										{label : qsTr("bsts - linear trend model - regression + lag"), value: "bstsLinearLag"},
										{label : qsTr("bsts - autoregressive model"), value: "bstsAr"},
										{label : qsTr("bsts - autoregressive model - regression"), value: "bstsArReg"},
										{label : qsTr("bsts - autoregressive model - regression + lag"), value: "bstsArRegLag"},
										{label : qsTr("prophet"), value: "prophet"},
										{label : qsTr("prophet - regression"), value: "prophetReg"},
										{label : qsTr("prophet - regression + lag"), value: "prophetRegLag"},
										{label : qsTr("xgboost - regression"), value: "xgboostReg"},
										{label : qsTr("xgboost - regression + lag"), value: "xgboostRegLag"},
										{label : qsTr("bart - regression"), value: "bartReg" },
										{label : qsTr("bart - regression + lag"), value: "bartRegLag"},
										{label : qsTr("bart - stack"), value: "bartStackReg"}



										]
									}]
								}
								AssignedVariablesList
								{
									name: "selectedModels"
									id: selectedModels

								}


							}



						}


		Group
		{

			title: qsTr("Evaluation Metrics")
			CheckBox{ name: "metricCrps"; 		label: qsTr("Cont. ranked probability score"); checked: true}
			CheckBox{ name: "metricDss"; 		label: qsTr("Dawid–Sebastiani score"); checked: true}
			CheckBox{ name: "metricLog"; 		label: qsTr("Log score"); checked: true}
			CheckBox{ name: "metricCoverage";	label: qsTr("Coverage"); checked: true}
			CheckBox{ name: "metricBias"; 		label: qsTr("Bias"); checked: true}
			CheckBox{ name: "metricPit";		label: qsTr("Probability integral transform"); checked: true}
			CheckBox{ name: "metricMae"; 		label: qsTr("Mean absolute error"); checked: true}
			CheckBox{ name: "metricRmse"; 		label: qsTr("Root mean squared error"); checked: true}
			CheckBox{ name: "metricR2"; 		label: qsTr("R²"); checked: true}

		}


		
	}


	Section
	{
		title: qsTr("Prediction Plots")
		Group
		{
			title: qsTr("Models to plot")
			VariablesForm

			{

				preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
				AvailableVariablesList
				{

					name: "fromR"
					source: [ { rSource: "plottableModelsQml" } ]
				}
				AssignedVariablesList
				{
					//height: 200
					name: "modelsToPlot"

				}
			}
			//CheckBox{name: "modelsToPlotCredibleInterval"; label: qsTr("Show credible interval")}

		}
	}


	Section
	{
		title: qsTr("Bayesian Model Averaging")
		CheckBox
		{
			name: "checkPerformBma"
			label: "Perform BMA"
			id: doBMA
			//checked: true

			RadioButtonGroup
			{
				name: "bmaMethod"
				title: qsTr("Method")
				RadioButton{ value: "bmaMethodEm"; label: qsTr("Expectation–maximization")}
				RadioButton{ value: "bmaMethodGibbs"; label: qsTr("Gibbs sampling")}
			}
			RadioButtonGroup
			{
				name: "bmaTestPeriod"
				title: "Evaluation Method"
				RadioButton{ value: "bmaTestNextSlice"; label: qsTr("Next test slice"); checked: true}
				RadioButton
				{
					value: "bmaSameSlice"
					label: qsTr("Same test slice")
					CIField { name: "bmaTestProp"; label: qsTr("Last");afterLabel: qsTr("% of data");defaultValue: 30;decimals:0;fieldWidth: 30}
				}
			}
		}
		Group
		{
			title: qsTr("Tables")
			CheckBox{
				name: "bmaWeightsTable"
				enabled: doBMA.checked
				label: qsTr("Model weights")
				CheckBox{name: "bmaWeightsTablePerSlice"; label: qsTr("Show per test slice");checked: true}
			}

		}

	}


	Section
	{
		title: qsTr("Future Prediction")


		VariablesForm
		{
			preferredHeight: jaspTheme.smallDefaultVariablesFormHeight/2
			//title: qsTr("Models")
			AvailableVariablesList
				{

					name: "futurePredictionModels"
					source:  [ { rSource: "plottableModelsQml" } ]

				}
				AssignedVariablesList
				{

					name: "selectedFuturePredictionModel"
					singleVariable: true
					height: 30

				}

		}


		Group
		{
			RadioButtonGroup
			{
				title: qsTr("Prediction horizon")
				name: "futurePredPredictionHorizon"


				//RadioButton
				//{
				//	name: "trainingIndicator"
				//	label: qsTr("Training indicator")
				//	checked: true
				//}

				//RadioButton
				//{
				//	name: "timepoints"
				//	label: qsTr("Time points")
				//	childrenOnSameRow: true
					IntegerField{name: "futurePredictionPoints"; afterLabel: qsTr("data points");min: 0;defaultValue: 0}
				//	checked: true
				//}

				//RadioButton
				//{
				//	name: "days"
				//	label: qsTr("Days:")
				//	childrenOnSameRow: true
				//	IntegerField{name: "futurePredictionDays"; min: 0; defaultValue: 0}
//
				//}
				
			}

			RadioButtonGroup
			{
				title: qsTr("Training period")
				name: "futurePredTrainingPeriod"

				RadioButton
				{
					value: "last"
					checked: true
					label: qsTr("Last")
					childrenOnSameRow: true
						IntegerField{name: "futurePredTrainingPoints"; afterLabel: qsTr("data points"); defaultValue: 200}
				}
				RadioButton{name: "all"; label: qsTr("All data points")}

		}
	}
	CheckBox
	{
		name: "checkFuturePredictionPlot"
		label: "Future prediction plot"
		checked: false
		CheckBox
		{
			name: "futurePredSpreadPointsEqually"
			label: qsTr("Spread points equally")
			checked: true
		}
	}

	}

	Section
	{
		title: qsTr("Advanced Options")

		CheckBox{name: 'parallelComputation'; label: 'Parallel model computation';checked: true}


		IntegerField{name: "resampleSkip"; label: qsTr("Skip between training slices");defaultValue: resampleForecastHorizon.value}



	}




}
