package com.cosmetic.gg.repository.attribute;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.cosmetic.gg.entity.attribute.ValueDetail;

@Repository
public interface ValueDetailRepository extends JpaRepository<ValueDetail, String>{
	
	@Query(value = "SELECT * FROM value_detail a INNER JOIN product_item t ON t.value_detail_id=a.id " +
			"WHERE (a.id=:key OR t.product_id=:key)", nativeQuery = true)
	List<ValueDetail> findByKey(@Param("key") String key);

	@Query( value = "SELECT * FROM value_detail t INNER JOIN product_item t2 ON t2.value_detail_id=t.id " +
			"WHERE t2.id=:id "
			, nativeQuery = true)
	ValueDetail getByProductItem(@Param("id") String id);
	
	@Query( value = "SELECT t.id AS id, t.import_price AS importPrice, t.sell_price AS sellPrice, t.import_quantity AS importQuantity, " +
			"t.sell_quantity AS sellQuantity, t.status As status, t.unit AS unit, t.description AS description, t.image AS image, " +
			"t2.id AS productItemId, t2.value AS value " +
			"FROM value_detail t INNER JOIN product_item t2 ON t.id=t2.value_detail_id " +
			"WHERE t2.product_id=:id "
			, nativeQuery = true)
	List<Object> valueDetail(@Param("id") String id);
	
	@Query( value = "SELECT t.id AS id, t.import_price AS importPrice, t.sell_price AS sellPrice, t.import_quantity AS importQuantity, " +
			"t.sell_quantity AS sellQuantity, t.status As status, t.unit AS unit, t.description AS description, t.image AS image, " +
			"t2.id AS productItemId, t2.value AS value " +
			"FROM value_detail t INNER JOIN product_item t2 ON t.id=t2.value_detail_id " +
			"WHERE t2.id=:id "
			, nativeQuery = true)
	Object getvalueDetailByProductItem(@Param("id") String id);
	
	@Query( value = "SELECT * FROM value_detail t INNER JOIN product_item t2 ON t.id=t2.value_detail_id " +
			"WHERE t2.id=:id "
			, nativeQuery = true)
	ValueDetail getvalueDetailByProductItem2(@Param("id") String id);
	
	@Query( value = "SELECT t.id AS id, t.import_price AS importPrice, t.sell_price AS sellPrice, t.import_quantity AS importQuantity, " +
			"t.sell_quantity AS sellQuantity, t.status As status, t.unit AS unit, t.description AS description, t.image AS image, " +
			"t2.value AS value, t2.product_id AS productId " +
			"FROM value_detail t INNER JOIN product_item t2 ON t.id=t2.value_detail_id " +
			"WHERE t.id=:id "
			, nativeQuery = true)
	Object detail(@Param("id") String id);
}
