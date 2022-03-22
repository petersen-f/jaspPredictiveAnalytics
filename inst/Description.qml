import QtQuick		2.12
import JASP.Module	1.0

Description
{
	name		: "jaspPredictiveAnalytics"
	title		: qsTr("Predictive Analytics")
	description	: qsTr("This module offers predictive analytics.")
	version		: "0.1"
	author		: "JASP Team"
	maintainer	: "JASP Team <info@jasp-stats.org>"
	website		: "https://jasp-stats.org"
	license		: "GPL (>= 2)"

	Analysis
	{
	    title: "Predictive Analytics"
	    func: "predictiveAnalytics"
		qml: 'predictiveAnalytics.qml'
	}
}
