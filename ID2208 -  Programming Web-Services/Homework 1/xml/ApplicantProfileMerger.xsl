<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	xmlns:ap="http://www.ApplicantProfile.se" version="1.0">

	<xsl:template match="/">
		<xsl:element name="ap:ApplicantProfile">
			<xsl:element name="ap:GeneralInfo">
				<xsl:copy-of select="document('IntermediaryCv.xml')/ap:GeneralInfo/*" />
			</xsl:element>
			<xsl:element name="ap:StudiesInfo">
				<xsl:copy-of select="document('IntermediaryTranscript.xml')/ap:StudiesInfo/*" />
			</xsl:element>
			<xsl:element name="ap:EmploymentInfo">
				<xsl:copy-of
					select="document('IntermediaryEmploymentRecord.xml')/ap:EmploymentInfo/*" />
			</xsl:element>
		</xsl:element>
	</xsl:template>
</xsl:stylesheet>