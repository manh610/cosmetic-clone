package com.cosmetic.gg.repository.product;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.cosmetic.gg.entity.product.Brand;


@Repository
public interface BrandRepository extends JpaRepository<Brand, String> {

	@Query( value = "SELECT * FROM brand t WHERE (t.code=:key or t.id=:key) AND t.status = 'ACTIVE'", nativeQuery = true)
	Brand findByKey(@Param("key") String key);
	
	@Query(value = "SELECT * FROM brand t WHERE " +
		    "(CASE WHEN :status IS NOT NULL THEN t.status=:status ELSE (t.status='ACTIVE' or t.status='DELETED') END) AND " +
			"(CASE WHEN :keyword IS NOT NULL AND :keyword <> '' THEN " +
			"(t.code REGEXP :keyword " +
			"OR t.name REGEXP :keyword) " +
			"ELSE (t.id IS NOT NULL) END)" +
			"ORDER BY t.name ASC, t.code ASC", nativeQuery = true)
	List<Brand> search(@Param("keyword") String keyword,
			@Param("status") String status);
}
