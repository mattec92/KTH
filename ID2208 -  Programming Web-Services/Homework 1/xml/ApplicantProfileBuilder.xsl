<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	xmlns:ap="http://www.ApplicantProfile.se" xmlns:cv="http://www.ShortCv.se"
	xmlns:t="http://www.Transcript.se" xmlns:e="http://www.EmploymentInfo.se"
	xmlns:c="http://www.CompanyInfo.se" version="1.0">

	<xsl:template match="/">
		<xsl:element name="ap:ApplicantProfile">
			<xsl:element name="ap:GeneralInfo">
				<xsl:element name="ap:FirstName">
					<xsl:value-of select="document('ShortCv.xml')//cv:FirstName" />
				</xsl:element>
				<xsl:element name="ap:LastName">
					<xsl:value-of select="document('ShortCv.xml')//cv:LastName" />
				</xsl:element>
				<xsl:element name="ap:PersonNumber">
					<xsl:value-of select="document('ShortCv.xml')//cv:PersonNumber" />
				</xsl:element>
			</xsl:element>
			<xsl:element name="ap:StudiesInfo">
				<xsl:element name="ap:University">
					<xsl:value-of select="document('Transcript.xml')//t:University" />
				</xsl:element>
				<xsl:element name="ap:Degree">
					<xsl:value-of select="document('Transcript.xml')//t:Degree" />
				</xsl:element>
				<xsl:element name="ap:Year">
					<xsl:value-of select="document('Transcript.xml')//t:Year" />
				</xsl:element>
				<xsl:element name="ap:GPA">
					<xsl:value-of
						select="sum(document('Transcript.xml')//t:Grade) div count(document('Transcript.xml')//t:Grade)" />
				</xsl:element>
				<xsl:element name="ap:Courses">
					<xsl:for-each select="document('Transcript.xml')//t:Course">
						<xsl:element name="ap:Course">
							<xsl:element name="ap:Name">
								<xsl:value-of select="t:Name" />
							</xsl:element>
							<xsl:element name="ap:Code">
								<xsl:value-of select="t:Code" />
							</xsl:element>
							<xsl:element name="ap:Grade">
								<xsl:value-of select="t:Grade" />
							</xsl:element>
						</xsl:element>
					</xsl:for-each>
				</xsl:element>
			</xsl:element>
			<xsl:element name="ap:EmploymentInfo">
				<xsl:for-each select="document('EmploymentRecord.xml')//e:Employment">
					<xsl:variable name="currentCompanyName" select="e:CompanyName" />
					<xsl:element name="ap:Employment">
						<xsl:element name="ap:Role">
							<xsl:value-of select="e:Role" />
						</xsl:element>
						<xsl:element name="ap:StartDate">
							<xsl:value-of select="e:StartDate" />
						</xsl:element>
						<xsl:element name="ap:EndDate">
							<xsl:value-of select="e:EndDate" />
						</xsl:element>
						<xsl:element name="ap:Company">
							<xsl:element name="ap:Name">
								<xsl:value-of select="e:CompanyName" />
							</xsl:element>
							<xsl:element name="ap:CompanyNumber">
								<xsl:value-of
									select="document('CompanyInfo.xml')//c:Company[c:Name = $currentCompanyName]/c:CompanyNumber" />
							</xsl:element>
							<xsl:element name="ap:PhoneNumber">
								<xsl:value-of
									select="document('CompanyInfo.xml')//c:Company[c:Name = $currentCompanyName]/c:PhoneNumber" />
							</xsl:element>
						</xsl:element>
					</xsl:element>
				</xsl:for-each>
			</xsl:element>
		</xsl:element>
	</xsl:template>
</xsl:stylesheet>