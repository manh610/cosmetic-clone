package com.cosmetic.gg.repository.supply;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.cosmetic.gg.entity.supply.Supplier;

@Repository
public interface SupplierRepository extends JpaRepository<Supplier, String>{

	@Query(value = "SELECT * FROM supplier t WHERE (t.id=:key OR t.email=:key OR t.phone=:key OR t.code=:key) "
		    , nativeQuery = true)
	Supplier findByKey(@Param("key") String key);
	
	@Query(value = "SELECT * FROM supplier t WHERE " +
			"(CASE WHEN :status IS NOT NULL THEN t.status=:status ELSE (t.status <> '') END) AND " +
			"(CASE WHEN :keyword IS NOT NULL AND :keyword <> '' THEN " +
		    "(t.code REGEXP :keyword " +
		    "OR t.email REGEXP :keyword " +
		    "OR t.phone REGEXP :keyword) " +
		    "ELSE (t.id IS NOT NULL) END) ORDER BY t.code ASC " 
			, nativeQuery = true)
	List<Supplier> search(@Param("keyword") String keyword,
						@Param("status") String status);
}
