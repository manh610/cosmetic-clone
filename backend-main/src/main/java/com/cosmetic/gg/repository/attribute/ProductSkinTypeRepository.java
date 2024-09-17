package com.cosmetic.gg.repository.attribute;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.cosmetic.gg.entity.attribute.ProductSkinType;


@Repository
public interface ProductSkinTypeRepository extends JpaRepository<ProductSkinType, String>{

	@Query(value = "SELECT * FROM product_skin_type a WHERE (a.id=:key OR a.product_id=:key)", nativeQuery = true)
	List<ProductSkinType> findByKey(@Param("key") String key);
}
