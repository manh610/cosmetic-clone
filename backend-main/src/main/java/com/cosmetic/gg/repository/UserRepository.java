package com.cosmetic.gg.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.cosmetic.gg.entity.User;

@Repository
public interface UserRepository extends JpaRepository<User, String>{

	@Query(value = "SELECT * FROM user t WHERE (t.id=:key OR t.username=:key OR " +
		    "t.email=:key OR t.phone=:key OR t.citizen_number=:key)", nativeQuery = true)
	User findByKey(@Param("key") String key);
	
	@Query(value = "SELECT " +
			"a.id AS id, a.username AS username, a.email AS email, a.phone AS phone, " +
			"a.citizen_number AS citizenNumber, a.given_name AS givenName, a.family_name AS familyName, " +
			"a.gender AS gender, a.dob AS dob, a.country AS country, a.password AS password, " +
			"a.user_rank AS userRank, a.avatar AS avatar, a.role_id AS roleId, a.delivery_unit_id AS deliveryUnitId,  " +
			"a.status AS status, a.description AS description, " +
			"b.name AS roleName, d.name AS deliveryUnitName " +
			"FROM user AS a INNER JOIN role b ON a.role_id=b.id " +
			"INNER JOIN delivery_unit d ON a.delivery_unit_id=d.id " +
			"WHERE (a.id=:id AND a.status='ACTIVE')",
		    nativeQuery = true)
	Object detailV1(@Param("id") String id);
	
	@Query(value = "SELECT " +
			"a.id AS id, a.username AS username, a.email AS email, a.phone AS phone, " +
			"a.citizen_number AS citizenNumber, a.given_name AS givenName, a.family_name AS familyName, " +
			"a.gender AS gender, a.dob AS dob, a.country AS country, a.password AS password, " +
			"a.user_rank AS userRank, a.avatar AS avatar, a.role_id AS roleId, a.delivery_unit_id AS deliveryUnitId,  " +
			"a.status AS status, a.description AS description, " +
			"b.name AS roleName " +
			"FROM user AS a INNER JOIN role b ON a.role_id=b.id " +
			"WHERE (a.id=:id AND a.status='ACTIVE')",
		    nativeQuery = true)
	Object detailV2(@Param("id") String id);
	
	@Query(value = "SELECT * FROM user a WHERE " +
		    "(CASE WHEN :status IS NOT NULL THEN a.status=:status ELSE (a.status <> '') END) AND " +
		    "(CASE WHEN :userRank IS NOT NULL THEN a.user_rank=:userRank ELSE (a.user_rank <> '') END) AND " +
		    "(CASE WHEN :roleId IS NOT NULL THEN a.role_id=:roleId ELSE (1=1) END) AND " +
		    "(CASE WHEN :keyword IS NOT NULL AND :keyword <> '' THEN " +
		    "(a.username REGEXP :keyword " +
		    "OR a.email REGEXP :keyword " +
		    "OR a.phone REGEXP :keyword " +
		    "OR CONCAT(a.given_name,' ', a.family_name) REGEXP :keyword " +
		    "OR a.citizen_number REGEXP :keyword) " +
		    "ELSE (a.id IS NOT NULL) END) " +
		    "ORDER BY a.created_at DESC, a.username ASC, a.family_name ASC, a.given_name ASC LIMIT :pageSize OFFSET :pageIndex",
		    nativeQuery = true)
	List<User> search(@Param("keyword") String keyword,
                      @Param("status") String status,
                      @Param("roleId") String roleId,
                      @Param("userRank") String userRank,
                      @Param("pageIndex") Integer pageIndex,
                      @Param("pageSize") Integer pageSize);
	
	@Query(value = "SELECT count(*) FROM user a WHERE " +
		      "(CASE WHEN :status IS NOT NULL THEN a.status=:status ELSE (1=1) END) AND " +
		      "(CASE WHEN :userRank IS NOT NULL THEN a.user_rank=:userRank ELSE (1=1) END) AND " +
		      "(CASE WHEN :roleId IS NOT NULL THEN a.role_id=:roleId ELSE (1=1) END) AND " +
		      "(CASE WHEN :keyword IS NOT NULL AND :keyword <> '' THEN " +
		      "(a.username REGEXP :keyword " +
		      "OR a.email REGEXP :keyword " +
		      "OR a.phone REGEXP :keyword " +
		      "OR CONCAT(a.given_name, ' ', a.family_name) REGEXP :keyword " +
		      "OR a.citizen_number REGEXP :keyword) " +
		      "ELSE (a.id IS NOT NULL) END) ", nativeQuery = true)
	Integer cntUser (@Param("keyword") String keyword,
                      @Param("status") String status,
                      @Param("roleId") String roleId,
                      @Param("userRank") String userRank);
	
	
	
	@Query( value = "SELECT * FROM user t INNER JOIN user_discount t1 ON t1.user_id=t.id " +
			"WHERE t1.discount_id=:id "
			, nativeQuery = true)
	List<User> getUserByDiscount(@Param("id") String id);
}
