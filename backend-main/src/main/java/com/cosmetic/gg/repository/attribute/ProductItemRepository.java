package com.cosmetic.gg.repository.attribute;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.cosmetic.gg.entity.attribute.ProductItem;

@Repository
public interface ProductItemRepository extends JpaRepository<ProductItem, String>{

	@Query(value = "SELECT * FROM product_item t WHERE (t.id = :key OR t.product_id = :key OR t.value_detail_id = :key)", nativeQuery = true)
	ProductItem findByKey(@Param("key") String key);
	
	@Query(value = "SELECT * FROM product_item t WHERE (t.id = :key OR t.product_id = :key OR t.value_detail_id = :key)", nativeQuery = true)
	List<ProductItem> findByKeys(@Param("key") String key);
	
	
	
	
	
	@Query( value = "SELECT * FROM product_item t1 INNER JOIN product t2 ON t2.id=t1.product_id " +
			"INNER JOIN value_detail t3 ON t3.id=t1.value_detail_id " +
			"WHERE (t1.id=:id AND t2.status='STOCK' AND t3.status='STOCK')"
			, nativeQuery = true)
	ProductItem checkExist(@Param("id") String id);
}
