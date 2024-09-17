package com.cosmetic.gg.repository.address;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.cosmetic.gg.entity.address.DeliveryUnit;

@Repository
public interface DeliveryUnitRepository extends JpaRepository<DeliveryUnit, String>{

	@Query( value = "SELECT * FROM delivery_unit t WHERE (t.code=:key or t.id=:key " +
			"or t.email=:key or t.phone=:key)" 
			, nativeQuery = true)
	DeliveryUnit findByKey(@Param("key") String key);
	
	@Query( value = "SELECT * FROM delivery_unit t WHERE " +
			"(CASE WHEN :deliveryType IS NOT NULL THEN t.delivery_type=:deliveryType ELSE (t.delivery_type <> '') END) AND " +
			"(CASE WHEN :keyword IS NOT NULL AND :keyword <> '' THEN " +
			"(t.code REGEXP :keyword " +
			"OR t.name REGEXP :keyword) " +
			"ELSE (t.id IS NOT NULL) END)" +
			"ORDER BY t.name ASC, t.code ASC", nativeQuery = true)
	List<DeliveryUnit> search(@Param("keyword") String keyword,
								@Param("deliveryType") String deliveryType);
}
