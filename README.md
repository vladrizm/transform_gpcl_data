# transform_gpcl_data

This R code is used to transform **public data** from [Home - Gladstone Ports Corporation (gpcl.com.au)](https://www.gpcl.com.au/).

Data is available through their 'Cargo Statistic Selections' [page](https://content3.gpcl.com.au/viewcontent/CargoComparisonsSelection/ "GPCL Report").

Report used is filtered for 'Combined - Origin & Destination' and every month of the year.

**Notes:**

-   Please allow up to **5 working days** from the last day of the previous month for the latest shipping statistics to become available online.

-   If this code is used from the **office location** you will have to run additional code lines to set up http and ***https proxy***. To properly set up your password for the both options, follow examples from this [instructions](https://www.cyberciti.biz/faq/unix-linux-export-variable-http_proxy-with-special-characters/ "Special Characters").

-   Good [guide](https://www.minitool.com/news/environment-variables-windows-10-005.html "Env Variables") how to set environment variables Windows 10.

-   \* Vessel has exported/imported more than one cargo. Vessel count applied only to one commodity.

    Dual berthing vessels will only be counted as one visit.

    By publishing this document you have acknowledged and accepted GPC's Legal policies, copyright, disclaimer and privacy requirements.
