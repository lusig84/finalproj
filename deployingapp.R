install.packages('rsconnect')
rsconnect::setAccountInfo(name='zenyakamdar', token='400FB8E4E6D4B736988A91F9FBAA0321', secret='qWyxOEpMqQzJeUYTl4WaD7loYvloeWAv5oP7TEkP')
library(rsconnect)
rsconnect::deployApp('path/to/your/app')
