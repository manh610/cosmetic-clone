package com.cosmetic.gg.repository.address;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.cosmetic.gg.entity.address.Province;

@Repository
public interface ProvinceRepository extends JpaRepository<Province, String>{

	@Query(value = "SELECT * FROM provinces t WHERE " +
			"(CASE WHEN :name IS NOT NULL AND :name <> '' THEN " +
			"t.full_name REGEXP :name " +
			"ELSE (t.id IS NOT NULL) END)" +
			"ORDER BY t.name ASC", nativeQuery = true)
	List<Province> findProvinces(@Param("name") String name);
}
