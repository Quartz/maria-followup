Latest data updated on *August 30, 2018*

## Adding new cases

You need to first download a copy of this repo to your computer. After you are done with the process described below, submit a pull request. 

### 1. Getting cases from Google Form responses

[Las víctimas de María (Responses)](https://docs.google.com/spreadsheets/d/1FK0j919EveJg6HJI_2139uQrX4W-jHX6bcBRAqQVSeI/edit#gid=1388179220)

If you don't have permission, ask Ana Campoy, Omaya Sosa, or Larry Fenn to help.

* Export the sheet directly:

 `File > Download as ... > Microsoft Excel(.xlsx)` 
 
* Find the lastest case (by timestamp) in `data/Las víctimas de María (Responses).xlsx` and delete all the cases up to that one in the spreadsheet you just downloaded. Save it.

* Replace the file `data/Las víctimas de María (Responses).xlsx` with the one you downloaded and newly saved.



### 2. Generate bitacoras

Run `getBitacoras.R` 

### 3. Get information from bitacoras

Reporters make phone calls using the bitacoras in the folder `bitacoras/Empty/` and gather the filled-out bitacoras to the folder `bitacoras/Filled/`

Then run script `processBitacoras.R` 


### 4. Copy edits and translation

Use the file `output/new_cases_copy.xlsx` for copy edits and translation.

* Spanish copy editing for column `text_field_es`
* Translate the column `text_field_es` to column `text_field_en` and copy edit `text_field_en`
* Save and replace the exising `output/new_cases_copy.xlsx`

### 5. Merge it back into the database

Run `merge.R`

### 6. Submit pull request
