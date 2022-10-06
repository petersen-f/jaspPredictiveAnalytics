import QtQuick 2.11
import QtQuick.Layouts 1.3
import JASP.Controls 1.0
import JASP.Widgets 1.0

Form
{
    VariablesForm
    {
        AvailableVariablesList	{ name: "allVariablesList" }
        AssignedVariablesList	{ name: "variables";	title: qsTr("Control Variables");	suggestedColumns: ["scale"]}
    }






    IntegerField
    {
        name: "previousDataPoints"
        Layout.columnSpan: 2
        label: qsTr("Only show previous")
        defaultValue: 10000
        afterLabel: "data points"
    }


    CheckBox
    {
        name: "overallControlSummaryTable"
        Layout.columnSpan: 1
        label: qsTr("Overall control summary table")
        checked: true
        CheckBox{name : "transposeOverallTable"; label: qsTr("Transpose table")}
        CheckBox{name: "orderTableByOutBound"; label: qsTr("Order table by proportion")}

    }





    CheckBox
    {
        name: "outBoundOverallPlotCheck"
        label: qsTr("Overall control plot")

        checked: true
        //CheckBox{name: "summaryPlotIndividualVars"; label: qsTr("plot individual variables")}

        RadioButtonGroup
            	{
                	name: "outBoundOverallPlotLineType"
                	radioButtonsOnSameRow: true
                    id: overollPlotType
                	RadioButton { value: "points";	label: qsTr("Points") }
                	RadioButton { value: "line";	label: qsTr("Line") }
                	RadioButton { value: "both";	label: qsTr("Both");	checked: true }
            	}

                CheckBox
                {
                    name: "outBoundOverallPlotJitterCheck"
                    label: qsTr("Add Jitter")
                    enabled : overollPlotType.value == "points" || overollPlotType.value == "both"
                }
        RadioButtonGroup
        {

            name: "outBoundOverallPlotMetricChoice"
            title: qsTr("Out-of-control metric")
            radioButtonsOnSameRow: false
            RadioButton{value: "number"; label: qsTr("Number");checked: true}
            RadioButton{value: "proportion"; label: qsTr("Proportion")}
        }


    }
    Section
    {
        title: qsTr("Proportion estimation")

        IntegerField
        {
            name: "multiBinWindow"
            min: 1
            defaultValue: 1
            label: qsTr("Summarise after every")
            afterLabel: qsTr("data points")
        }
        IntegerField{name: "multiBinomDraws"; defaultValue: 100; label: qsTr("MCMC draws")}

        CheckBox
        {
            name: "multiBinaryCheckPlot"
            label: qsTr("Multivariate binary control plot")
            checked: true
        }
    }
    Section
    {
        title: qsTr("Prediction")

        IntegerField
        {
            name: "predictionHorizon"
            Layout.columnSpan: 2
            id: predictionHorizon
            defaultValue: 0
            label: qsTr("Prediction horizon")
        }
        CheckBox
        {
            name: "predictionTimePlot"
            label: qsTr("Plot predictions")
            enabled: predictionHorizon.value > 0
        }
    }
}
