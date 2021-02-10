module Bank

type Account =
    | Balance of int
    | Overdrawn of int
    | Empty

type Password = string

type Name = string

type Customer =
    { Name: Name
      Password: Password
      Account: Account }

type Action =
    | Withdraw of int
    | Deposit of int

type Session =
    | Valid of Customer
    | BadPassword

type TransactionResult =
    | AccountUpdated of Customer
    | Failed

let abs intVal =
    if intVal < 0 then
        intVal * -1
    else
        intVal

// Makes an Empty account.
let makeAccount () = Empty


// Withdraws the given amount from an account.
let withdraw amount customerAccount =
    // ensure that amount is positive
    let amount = abs (amount)

    match customerAccount with
    | Balance b ->
        if b < amount then
            Overdrawn(abs (b - amount))
        elif b = amount then
            Empty
        else
            Balance(b - amount)
    | Overdrawn o -> Overdrawn(o - amount)
    | Empty -> Overdrawn(amount)

// Deposits the given amount into an account.
let deposit amount customerAccount =
    // ensure that amount is positive
    let amount = abs (amount)

    match customerAccount with
    | Balance b -> Balance(b + amount)
    | Overdrawn o ->
        if o > amount then Overdrawn(o - amount)
        elif o = amount then Empty
        else Balance(abs (o - amount))
    | Empty -> Balance(amount)




// Makes a customer with the given name and password. Must be an empty customer.
let makeCustomer name password =
    let customer =
        { Name = name
          Password = password
          Account = makeAccount () }

    customer

// Makes a session for a Customer. A session is `Valid` iff the password given
// matches the customer's password. `BadPassword` is returned otherwise.
let makeSession password customer =
    if password = customer.Password then
        Valid(customer)
    else
        BadPassword


// Performs the specified action on a customer's account and returns an updated
// customer iff the session is valid. Returns `Failed` otherwise.
let performTransaction action session =
    match session with
    | BadPassword -> Failed
    | Valid v ->
        // update the account based on the action
        let updatedAccount =
            match action with
            | Withdraw w -> withdraw w v.Account
            | Deposit d -> deposit d v.Account

        // create a new customer with the updated info and return it
        let updatedCustomer =
            { Name = v.Name
              Password = v.Password
              Account = updatedAccount }

        AccountUpdated(updatedCustomer)
