# Maria's Dead follow up

Instructions and scripts for adding new cases for the Hurricane Maria project. The existing databases live at:

https://hurricanemariadead.com/

https://losmuertosdemaria.com/

### Recent updates:

Latest data updated on *Dec. 19, 2018*, **504 cases**

*August 30*, 2018, **487 cases**

## Adding new cases

You need to first download a copy of this repo to your computer. After you are done with the process described below, submit a pull request. 

### 1. Getting cases from Google Form responses

[Las víctimas de María (Responses)](https://docs.google.com/spreadsheets/d/1FK0j919EveJg6HJI_2139uQrX4W-jHX6bcBRAqQVSeI/edit#gid=1388179220)

If you don't have permission, ask Ana Campoy, Omaya Sosa, or Larry Fenn to help.

* Export the sheet directly:

 `File > Download as ... > Microsoft Excel(.xlsx)` 
 
* Delete all the cases in the spreadsheet up to the *Last Updated* date at the top of this Readme document you just downloaded. Save it.

* Put the newly-saved file into the `data` folder: `data/Las víctimas de María (Responses).xlsx`.


### 2. Generate bitacoras

Run `getBitacoras.R` 

### 3. Get information from bitacoras

- Inform Laura Candelas (CPI), who will coordinate verification phone calls using the bitacoras in the folder `bitacoras/Empty/` 
- Gather only the complete bitacoras to the folder `bitacoras/Filled/`

- Run script `processBitacoras.R` 


### 4. Copy edits and translation

Send the file file `data/forProcess/for_translation_copyedits.xlsx` to Laura and Ezequiel for copy edits and translation. They need to do three things:

* Spanish copy editing for column `text_field_es` (Laura)
* Translate the column `text_field_es` to column `text_field_en` and copy edit `text_field_en` (Ezequiel)
* Translate the column `causes_en` to column `causes_es` (Laura)

(The person who does copy edits should also do a general additional verification to make sure qualtitative information in `text_field` matches other basic info, such as gender (as inferred from name), clinical cause of death, mechanism of death, age, etc, and make corrections or flag cases accordingly. )

When getting back the file, please save it and replace the exising `data/forProcess/for_translation_copyedits.xlsx`

### 5. Merge it back into the database

Run `merge.R`

### 6. Submit pull request


