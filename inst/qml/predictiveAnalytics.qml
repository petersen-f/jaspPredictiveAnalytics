import QtQuick 2.11
import QtQuick.Layouts 1.3
import JASP.Controls 1.0
import JASP.Widgets 1.0

Form
{

	VariablesForm
	{

		AvailableVariablesList	{ name: "allVariablesList" }
		AssignedVariablesList	{ name: "dependent";	title: qsTr("Dependent Variable");	suggestedColumns: ["scale"];	singleVariable: true}
		AssignedVariablesList	{ name: "dates";		title: qsTr("Time");				suggestedColumns: ["nominal"];	singleVariable: true		}
	}


	Section
	{
		title: qsTr("Time Series Descriptives")
		
		Group
		{
			title: qsTr("Error Bound Selection Method")

			RadioButtonGroup
			{
				name: "errorBoundMethod"
				
				RadioButton
				{
					value: "manualBound"
					label: qsTr("Manual bounds")

					IntegerField{name: "controlMean"; label: qsTr("Control Mean"); defaultValue: Null; negativeValues: true }
					IntegerField{name: "controlError"; label: qsTr("Control Error"); defaultValue: 0; negativeValues: false }

				}
				RadioButton
				{
					value: "stdDevBound"
					label: qsTr("Standard Deviation Bound")
					IntegerField{name: "sigmaBound"; label: qsTr("σ threshold"); defaultValue: 2}
					CheckBox
					{
						name: "controlPeriodCheck"
						label: qsTr("Custom Control Period")
						DoubleField{name:"controlPeriod"; afterLabel: qsTr("time points"); defaultValue: 20}
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
				label: qsTr("Control plot show")
				RadioButtonGroup
            	{
                	name: "controlLineType"
                	radioButtonsOnSameRow: true
                	RadioButton { value: "points";	label: qsTr("Points") }
                	RadioButton { value: "line";	label: qsTr("Line") }
                	RadioButton { value: "both";	label: qsTr("Both");	checked: true }
            	}

				RadioButtonGroup
				{
					name: "xAxisLimit"
					title: "Y-Axis Limit:"
					radioButtonsOnSameRow: true
					RadioButton { value: "allData";	label: qsTr("All data") }
					RadioButton { value: "controlBounds";	label: qsTr("Control bounds") }
				}
				CheckBox
				{
					name: "zoomControlPlotCheck"
					label: "Focus on last"
					childrenOnSameRow: true
					IntegerField{name: "zoomControlLength"; afterLabel: qsTr("data points"); defaultValue: 0 }
					
				}
			}
		}

		
	}
	Section
	{
		title: qsTr("Binary Control Analysis")
		Group
		{
			CheckBox 
			{
				name: "binaryControlChartCheck"
				label: "Show binary control chart"

				DropDown
				{
					name: "binaryControlMethod"
					label: "Select Control Method"
					values: ["State Space", "beta distribution"]

				}
			}
		}
	}

	Section
	{
		title: qsTr("Control Prediction")
	}
	
}
