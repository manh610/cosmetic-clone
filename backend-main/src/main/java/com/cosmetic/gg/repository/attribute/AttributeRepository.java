package com.cosmetic.gg.repository.attribute;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.cosmetic.gg.entity.attribute.Attribute;

@Repository
public interface AttributeRepository extends JpaRepository<Attribute, String>{
	
	@Query( value = "SELECT * FROM attribute t WHERE (t.code=:key or t.id=:key) AND t.status = 'ACTIVE'", nativeQuery = true)
	Attribute findByKey(@Param("key") String key);

	@Query( value = "SELECT * FROM attribute t WHERE (CASE WHEN :status IS NOT NULL THEN t.status=:status ELSE (t.status='ACTIVE' or t.status='DELETED') END)", nativeQuery = true)
	List<Attribute> search(@Param("status") String status);
}
