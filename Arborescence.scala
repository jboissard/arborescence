package models

import play.api.db._
import play.api.Play.current

import anorm._
import anorm.SqlParser._

import java.util.Date

case class Arborescence(id: Pk[Long], name: String, level: Long, sub: Option[Long], position: Long)

object Arborescence{

  import play.Logger

  val table = "arborescence"

  val query:String = """
    SELECT id, name, level, sub, position
    FROM """+table+"""
  """

  /* get whole ascending branch */
  def branch(id: Long, sort: Boolean = true) : List[Arborescence] = {

    var sub:Option[Long] = Some(id)

    var arborescence:List[Arborescence] = List()

    while(sub.isDefined){

      val a = detail(sub.get)
      Logger.info(a.toString)

      if(a.isDefined){
        sub         = a.get.sub
        arborescence    = arborescence :+ a.get
      }
      else{
        sub = None
      }
    }

    // sort by level
    if(sort){
      arborescence.sortBy(_.level)
    }
    else{
      arborescence
    }
  }

  def list(id: Option[Long] = None): List[Arborescence] = {

    val q = query +
        {id.map{iid =>
        	" WHERE sub={sub}"
        }.getOrElse{""}} +
        " ORDER BY position"

    DB.withConnection{implicit c =>
      SQL(q)
      .on("sub" -> {id.getOrElse(0l))
      .as(parser *)
    }
  }

  def listJSON(list:List[Arborescence]):play.api.libs.json.JsValue = {
    import play.api.libs.json._

    var level:Long = 1 
    var b:String ="{"

    Arborescence.list().map{l =>
      
      if(level<l.level){
        b=b+{if(l.level>2){"{"}else{""}}+"\"children\": ["
      }

      if(level>l.level){
        b=b.dropRight(1)+{("]}"*(level-l.level).toInt)}
        if(l.level<2){
          b=b.dropRight(1)
        }
        b=b+","
      }

      val content:String = l.id.toString+l.level+" "+l.name
      b=b+{if(l.level>1){"{"}else{""}}+"\"label\": \""+content+"\""+{if(l.level>1){"}"}else{""}}+","

      level = l.level
    }
    
    b= b.dropRight(1)+"}"

    Json.parse(b)
  }


  def detail(id: Long): Option[Arborescence] = {
    DB.withConnection{implicit c =>
      SQL(query+" WHERE id={id}")
      .on('id -> id)
      .as(parser singleOpt)
    }
  }

  def detailFromPosition(position: Long): Option[Arborescence] = {
    DB.withConnection{implicit c =>
      SQL(query+" WHERE position={position}")
      .on('position -> position)
      .as(parser singleOpt)
    }
  }

  def listBetweenIds(id1: Long, id2: Long = 0): List[Long] = {
    
    var q_next:String = """SELECT id FROM """+table+"""
      WHERE 
        id NOT IN({id1},{id2})
        AND position
    """

    if(id2 == 0){
      q_next = q_next + 
          """ > (SELECT position FROM """+table+""" WHERE id={id1})"""
    }
    else{
      q_next = q_next + 
          """ BETWEEN (SELECT position FROM """+table+""" WHERE id={id1})
            AND (SELECT position FROM """+table+""" WHERE id={id2})
          """
    }

    DB.withConnection{implicit c =>
      val rows = SQL(q_next)
      .on(
        'id1 -> id1,
        'id2 -> id2
      )
      .apply()
      .map(row =>
        row[Long]("id")
      )
      .toList

      rows
    }
  }

  def listSubAfterId(id: Long, direction: Boolean): List[Long] = {
    
    val q:String = """SELECT id FROM """+table+"""
      WHERE 1
        AND position """+getDirectionString(direction, 1)+""" (SELECT position FROM """+table+""" WHERE id={id})
        AND level > (SELECT level FROM """+table+""" WHERE id={id})
    """

    DB.withConnection{implicit c =>
      val rows = SQL(q)
      .on(
        'id -> id
      )
      .apply()
      .map(row =>
        row[Long]("id")
      )
      .toList

      rows
    }
  }

  // returns a string with element separated with ,
  def listToSQL(l: List[Long]): Option[String] = {
    if(l.length==0){
      return None
    }

    var r:String="("

    l.map{a=>
      r=r+a.toString+","
    }


    r = r.dropRight(1)+")"

    Some(r)
  }

  val parser = {
    get[Pk[Long]]("id")~
    get[String]("name")~
    get[Long]("level")~
    get[Option[Long]]("sub")~
    get[Long]("position") map {
      case id~name~level~sub~position => Arborescence(id, name, level, sub, position)
    }
  }

  // if id isDefined -> returns last element of subcategory
  def last(id:Long = 0):Option[Arborescence] = {

    val q:String = query+
            "WHERE "+{if(id>0){"sub={id}"}else{"1"}}+
            " ORDER BY position DESC"+
            " LIMIT 0,1"

    //Logger.info(q)

    DB.withConnection{implicit c =>
      SQL(q)
      .on('id -> id)
      .as(parser singleOpt)
    }
  }

  /**********
  *
  *  alter
  * todo: 
  * add: add without caring of where, find right position (catch element), move
  * idea => do not touch attribute position anywhere but in method move!! <- avoids al lot of errors 
  *
  ***********/
  def insert(data: Arborescence){
    

    // get info about sub
    var sub:Option[Arborescence]     = None
    var level:Long             = 1
    var position:Long           = 1

    if(data.sub.isDefined){
      sub = detail(data.sub.get)

      level = sub.get.level + 1

      // get position
      val last_sub:Option[Arborescence] = detailNextPosition(sub.get)

      if(last_sub.isDefined){
        position = last_sub.get.position
      }
      // no previous entry in that level
      else{
        position = last().get.position + 1
      }
    }
    else{
      // get last position
      val lastElement = last()
      if(lastElement.isDefined){
        position = lastElement.get.position + 1
        
      }
    }

    // shift position
    DB.withConnection{implicit c =>
      SQL("UPDATE "+table+" SET position=position+1 WHERE position>={position}")
      .on('position -> position)
      .executeUpdate
    }

    // prepare query
    val query = 
      """
      INSERT INTO """+table+"""
      SET name={name}, level={level}, sub={sub}, position={position}
      """


    // insert/update entry
    DB.withConnection{implicit c =>
      SQL(query)
      .on(
        'name   -> data.name,
        'sub     -> data.sub,
        'level   -> level,
        'position -> position
      )
      .executeUpdate
    }
  }

  // todo: allow changing of sub: update sub, level and position! -> only one function : move(a,b)
  def update(data: Arborescence, id: Long){
    // prepare query
    val query:String = 
      "UPDATE "+
      table+
      " SET name={name} WHERE id={id}"


    // insert/update entry
    DB.withConnection{implicit c =>
      SQL(query)
      .on(
        'name   -> data.name,
        'id     -> id
      )
      .executeUpdate
    }
  }

  

  /*
    returns number of entries in submenus
  */
  def nSubs(id: Long): Option[Long] = {

    
    val qb:String = "SELECT COUNT(id) as c FROM "+table+" WHERE sub={sub}"

    try{
      DB.withConnection{implicit c=>
        val row = SQL(qb)
        .on('sub -> id)
        .apply().head
            
        Some(row[Long]("c"))
      }
    }
    catch{
      case _ : Throwable => None
    }
  }


  // translate direction to different strings
  def getDirectionString(direction:Boolean, t:Long = 0):String = {

    var r:List[String] = List()

    t match{
      case 1 => {r = List(">","<")}
      case 2 => {r = List("ASC","DESC")}
      case _ => {r = List("+","-")}
    }

    if(direction){
      r(0)
    }
    else{
      r(1)
    }

  }

  def detailNextPosition(a: Arborescence, direction: Boolean = true, boundary: Boolean = true): Option[Arborescence] = {
    
    val q_next:String = query+"""
      WHERE 1
        AND position"""+getDirectionString(direction, 1)+"""{position}
        AND level"""+{if(boundary){"<"}else{""}}+"""={level}
      ORDER BY position """+getDirectionString(direction, 2)+"""
      LIMIT 0,1
    """

    //
    //level={level}

    DB.withConnection{implicit c =>
      SQL(q_next)
      .on(
        'position   -> a.position,
        'level     -> a.level,
        'sub       -> a.sub
      )
      .as(parser singleOpt)
    }
  }

  /*
    a: origin vertex/node
    b: target node
  */
  def move(a: Arborescence, b:Arborescence){

    val direction:Boolean = {
      if(a.position>b.position){
        false
      }
      else{
        true
      }
    }

    // end of tail and end of tail target
    val c                 = detailNextPosition(a, true, true)
    val d                 = detailNextPosition(b, true, true)

    // get tails
    var ids:List[Long]   = List()
    var idsA:List[Long]   = List()

    if(c.isDefined){
      ids = listBetweenIds(a.id.get, c.get.id.get) :+ a.id.get
      Logger.info("origin (A) "+a.id.get+" and end of tail (C) "+c.get.id.get)
    }
    else{
      ids = listBetweenIds(a.id.get, 0) :+ a.id.get
    }

    if(d.isDefined){
      idsA = listBetweenIds(b.id.get, d.get.id.get) :+ b.id.get
      Logger.info("target (B) "+b.id.get+" and end of tail (D) "+d.get.id.get)
    }
    else{
      idsA = listBetweenIds(b.id.get, 0) :+ b.id.get  
    }
    // end tails

    // sumaary of operations
    Logger.info("tail (A - C) ids: "+ids.toString+", dpos: "+ids.size.toString)
    Logger.info("tail (B - D) "+idsA.toString+", dposA: "+idsA.size.toString)
    Logger.info("move tail of: "+getDirectionString(direction)+idsA.size.toString)
    Logger.info("move target tail of "+getDirectionString(!direction)+ids.size)

    // prepare queries
    val q1:String = "UPDATE "+table+" SET position=position"+getDirectionString(direction)+"{dposA} WHERE id IN"+listToSQL(ids).get+""
    val q2:String = "UPDATE "+table+" SET position=position"+getDirectionString(!direction)+"{dpos} WHERE id IN"+listToSQL(idsA).get+""

    // execute queries
    DB.withConnection{implicit c =>
      SQL(q1)
      .on(
        'dposA -> idsA.size
      )
      .executeUpdate

      SQL(q2)
      .on(
        'dpos -> ids.size
      )
      .executeUpdate  
    }
  }

  /*
    moves a item up or down, within same category
    @arg id: id
    @arg direction: indicates in which direction to move the object (0: down, 1: up) --> transformation d' = 1 - 2d (minus because going up is actually decreasing value of position)

    todo: when it is at the end (b undefined)
  */
  def moveOneStep(id: Long, direction: Boolean){
    val a = detail(id)

    if(a.isDefined){

      // target
      val b = detailNextPosition(a.get, direction)
      
      if(b.isDefined && b.get.level == a.get.level){
        move(a.get, b.get)
      }      
    }
  }

  def delete(id: Long){

    val a = detail(id)

    // if entry exists and no assoaicted subs
    val nSub = nSubs(id)
    Logger.info("nsub:"+nSub.toString)
    if(a.isDefined && nSub.isDefined && nSub.get==0){

      val qs:String = "UPDATE "+table+" SET position=position-1 WHERE position>{position}"

      DB.withConnection{implicit c =>
        SQL(qs)
        .on('position -> a.get.position)
        .executeUpdate
      }


      val q:String = "DELETE FROM "+table+" WHERE id={id}"

      DB.withConnection{implicit c =>
        SQL(q)
        .on('id -> id)
        .executeUpdate
      }
    }
  }

  def subSelect: Seq[(String, String)] = {
    var sub: Seq[(String, String)] = Seq(("","-"))

    list().map{l =>
      sub = sub :+ (l.id.get.toString,{">" * l.level.toInt}+" "+l.name )
    }

    sub
  }
}