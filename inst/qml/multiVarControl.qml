import QtQuick 2.11
import QtQuick.Layouts 1.3
import JASP.Controls 1.0
import JASP.Widgets 1.0

Form
{
    VariablesForm
    [
        AvailableVariablesList	{ name: "allVariablesList" }
        AssignedVariablesList	{ name: "variables";	title: qsTr("Control Variables");	suggestedColumns: ["scale"]}
    ]


    IntegerField{name: "previousDataPoints"; title: qsTr("Only show previous");defaultValue: 10000; afterLabel: "data points"}


    CheckBox
    {
        name: "overallControlSummaryTable"
        title: qsTr("Overall control summary table")
        checked: true
        CheckBox(name : "transposeOverallTable"; title: qsTr("Transpose table"))
        CheckBox{name: "orderTableByOutBound"; title: qsTr("Order table by error out-of-bound proportion")}

    }

    CheckBox
    {
        name: "outBoundOverallPlotCheck"
        title qsTr("Overall control plot")
        checked: true
        CheckBox{name: "summaryPlotIndividualVars"; title: qsTr("plot individual variables")}
        RadioButtonGroup
        {
            name: "outBoundOverallPlotMetricChoice"
            title: qsTr("Out-of-control metric")
            adioButtonsOnSameRow: true
            RadioButton{value: "number"; title: qsTr("Number");checked: true}
            RadioButton{value: "percent"; title: qsTr("Percent")}
        }
        RadioButtonGroup
            	{
                	name: "outBoundOverallPlotLineType"
                	radioButtonsOnSameRow: true
                	RadioButton { value: "points";	label: qsTr("Points") }
                	RadioButton { value: "line";	label: qsTr("Line") }
                	RadioButton { value: "both";	label: qsTr("Both");	checked: true }
            	}
    }
    Section
    {
        title: qsTr("Multivariate Binomial Control")

        IntegerField
        {
            name: "multiBinWindow"
            min: 1
            defaultValue: 1
            label: qsTr("Summarise after every")
            afterLabel: qsTr("data points")
        }
        IntegerField{name: multiBinomDraws, defaultValue: 500}
        
        CheckBox
        {
            name: "multiBinaryCheckPlot"
            title: qsTr("Multivariate binary control plot")
        }
    }
}