"odfDataType" <-
function(x)
{
   internalMode <- typeof(x)
   dataMode <- internalMode
   dataMode[dataMode == "character"] <- "string"
   dataMode[dataMode == "integer"] <- "float"
   dataMode[dataMode == "double"] <- "float"
   # possible office:value-type values are:
   #float, percentage, currency, date, time, boolean, string
   if (any(internalMode == "integer" & is.factor(x)))
      dataMode[internalMode == "integer" & is.factor(x)] <- "string"
   dataMode
}
