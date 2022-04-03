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
			DropDown
			{
				name: "errorBoundMethodDrop"
				id: methodSelection
				values:
 				[
    				{ label: "Manual Bounds", value: "manualBound"},
    				{ label: "Data Based", value: "stdDevBound"}
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
				IntegerField{name: "sigmaBound"; label: qsTr("σ threshold"); defaultValue: 2}
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
