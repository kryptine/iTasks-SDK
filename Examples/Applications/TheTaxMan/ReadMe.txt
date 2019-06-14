TheTaxMan example was created as a case study of a relatively small, yet realistic, scenario for the 
Dutch Tax Authorities. 

If you wish to customize it for your demo, then you should check the files in directory 'ExampleData':
(*) addresses.txt: 
    contains the credentials of the registered citizens. Format:
    
    <social security number>\t<first name>\t<surname>\t<postcode>\t<house number>\t<comment>\n
                   
    for simplicity a citizen account is the social security number with identical password

(*) officers.txt:
    contains the login names for tax officers; for simplicity their passwords are identical. Format:

    <account-name>\n

(*) real_estate_owners: 
    contains the registered real estate owners. 
    A real estate owner can be a citizen (identified by the social security number) or a 
    company (identified by the chamber of commerce number). Format:

    <postcode>\t<house number>\t<id>\t<comment>\n

(*) roofing_companies.txt:
    contains the registered roofing companies. Format:

    <coc number>\t<company name>\n

When running a demo, login as 'root', and perform the "SDS setup" administration tasks.
This will read the above files and create the appropriate SDS's for the demo.

You can then log out, and re-login as a citizen, company, or tax officer.
