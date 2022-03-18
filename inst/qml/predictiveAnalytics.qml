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
					IntegerField{name: "sigmaBound"; label: qsTr("σ threshold")}
					CheckBox
					{
						name: "controlPeriodCheck"
						label: qsTr("Custom Control Period")
						DoubleField{name:"controlPeriod"; afterLabel: qsTr("time points")}
					}
				}
			}
		}
		Group
		{
			title: "Control Plots"

			CheckBox{name:"controlPlotCheck"; label: qsTr("Control plot show")}
		}

		
	}
	Section
	{
		title: qsTr("Binary Control Analysis")
	}

	Section
	{
		title: qsTr("Control Prediction")
	}
	
}
