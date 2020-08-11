package ai.economicdatasciences.errorhandling

case class Employee(name: String, department: String){

    def lookupByName(name: String): Option[Employee] = ???

    val joeDepartment: Option[String] = lookupByName("joe").map(_.department)
    
}
