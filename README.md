# AgroFerrInvoiceTransformer
Transform AgroFerr Invoices by adding extra fields

### How to download
You must have installed:
- sbt  (scala build tool)
- git 

Go to a folder you want the program to be downloaded and then execute:

`git clone https://github.com/gabor-dorcsinecz/AgroFerrInvoiceTransformer`

then go into the folder you've just downloaded:

`cd AgroFerrInvoiceTransformer`

### How to update the program
`git pull`

### How to use
The program can transform induvidual files and entire directories. The resulting files
will be in a new folder called `out` next to the given file or folder

Transforming a single file:
`sbt "run the/path/to/file/name.xml"`

Transforming all xml files in a directory: 
`sbt "run the/path/of/the/folder"`
